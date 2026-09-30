;; Copyright (c) 2015 Grim Schjetne
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(in-package #:json-mop)

(defgeneric to-lisp-value (value json-type)
  (:documentation
   "Turns a value parsed by jzon into the appropriate
  Lisp type as specified by JSON-TYPE"))

(defmethod to-lisp-value ((value (eql 'null)) json-type)
  "When the value is JSON null, signal NULL-VALUE error"
  (error 'null-value :json-type json-type))

(defmethod to-lisp-value (value (json-type (eql :any)))
  "When the JSON type is :ANY, Pass the VALUE unchanged"
  value)

(defmethod to-lisp-value ((value hash-table) (json-type (eql :any)))
  "When the JSON type is :ANY, Pass the hash-table VALUE unchanged"
  value)

(defmethod to-lisp-value ((value string) (json-type (eql :string)))
  "Return the string VALUE"
  value)

(defmethod to-lisp-value ((value number) (json-type (eql :number)))
  "Return the number VALUE"
  value)

(defmethod to-lisp-value ((value integer) (json-type (eql :integer)))
  "Return the number VALUE"
  value)

(defmethod to-lisp-value ((value hash-table) (json-type (eql :hash-table)))
  "Return the hash-table VALUE"
  value)

(defmethod to-lisp-value ((value hash-table) (json-type cons))
  "Return the homogeneous hash-table VALUE"
  (destructuring-bind (hash-keyword out-type)
                      json-type
    (ecase hash-keyword
      (:hash-table
       (let ((out (make-hash-table :test 'equal :size (hash-table-size value))))
         (maphash (lambda (k v)
                    (setf (gethash k out) (to-lisp-value v out-type)))
                  value)
         out)))))

(defmethod to-lisp-value ((value vector) (json-type (eql :vector)))
  "Return the vector VALUE"
  value)

(defmethod to-lisp-value ((value vector) (json-type (eql :list)))
  "Return the list VALUE"
  (coerce value 'list))

(defmethod to-lisp-value (value (json-type (eql :bool)))
  "Return the boolean VALUE"
  (ecase value ((t) t) ((nil) nil)))

(defmethod to-lisp-value ((value vector) (json-type cons))
  "Return the homogeneous sequence VALUE"
  (map (ecase (first json-type)
         (:vector 'vector)
         (:list 'list))
       (lambda (item)
         (handler-case (to-lisp-value item (second json-type))
           (null-value (condition)
             (declare (ignore condition))
             (restart-case (error 'null-in-homogeneous-sequence
                                  :json-type json-type)
               (use-value (value)
                 :report "Specify a value to use in place of the null"
                 :interactive read-eval-query
                 value)))))
       value))

(defmethod to-lisp-value ((value hash-table) (json-type symbol))
  "Return the CLOS object VALUE"
  (json-to-clos value json-type))

;;; Streaming decoder
;;;
;;; Objects of JSON-SERIALIZABLE-CLASS classes, and homogeneous
;;; sequences and hash tables, are read event by event from a jzon
;;; parser, so that slots are set as their values come up. Any other
;;; value is read whole with JZON:PARSE-NEXT-ELEMENT and handed to
;;; TO-LISP-VALUE.
;;;
;;; Invariant: READ-JSON-VALUE only lets a NULL-VALUE error escape
;;; once the value has been read completely, so that a handler can
;;; carry on reading from the parser.

(defun json-class-p (json-type)
  (and (symbolp json-type)
       (typep (find-class json-type nil) 'json-serializable-class)))

(defun homogeneous-type-p (json-type keywords)
  (and (consp json-type)
       (member (first json-type) keywords)))

(defun skip-json-value (parser event)
  "Skip the value starting with EVENT in PARSER."
  (when (member event '(:begin-array :begin-object))
    (loop with depth = 1
          until (zerop depth)
          do (case (jzon:parse-next parser)
               ((:begin-array :begin-object) (incf depth))
               ((:end-array :end-object) (decf depth))))))

(defun find-json-slot (class key)
  "Return the most specific slot definition in CLASS with the JSON key KEY."
  (dolist (superclass (closer-mop:class-precedence-list class))
    (dolist (slot (closer-mop:class-direct-slots superclass))
      (when (and (typep slot 'json-serializable-slot)
                 (equal (json-key-name slot) key))
        (return-from find-json-slot slot)))))

(defun read-json-object (parser class-name &rest initargs)
  "Read the rest of a JSON object from PARSER into a fresh instance of
CLASS-NAME, setting each slot as its key comes up."
  (let* ((lisp-object (apply #'make-instance class-name initargs))
         (class (class-of lisp-object))
         (key-count 0))
    (loop for (event key) = (multiple-value-list (jzon:parse-next parser))
          until (eq event :end-object)
          do (let ((slot (find-json-slot class key)))
               (if slot
                   (handler-case
                       (progn
                         (setf (slot-value lisp-object
                                           (closer-mop:slot-definition-name slot))
                               (read-json-value parser (json-type slot)))
                         (incf key-count))
                     (null-value (condition)
                       (declare (ignore condition)) nil))
                   (skip-json-value parser (jzon:parse-next parser)))))
    (when (zerop key-count) (warn 'no-values-parsed
                                  :class-name class-name))
    (values lisp-object key-count)))

(defun read-homogeneous-sequence (parser json-type)
  "Read the rest of a JSON array from PARSER as the homogeneous
sequence type JSON-TYPE."
  (let ((items (loop with end = '#:end
                     for item = (handler-case
                                    (read-json-value parser (second json-type) end)
                                  (null-value (condition)
                                    (declare (ignore condition))
                                    (restart-case (error 'null-in-homogeneous-sequence
                                                         :json-type json-type)
                                      (use-value (value)
                                        :report "Specify a value to use in place of the null"
                                        :interactive read-eval-query
                                        value))))
                     until (eq item end)
                     collect item)))
    (ecase (first json-type)
      (:list items)
      (:vector (coerce items 'simple-vector)))))

(defun read-homogeneous-hash-table (parser json-type)
  "Read the rest of a JSON object from PARSER as the homogeneous
hash-table type JSON-TYPE."
  (loop with hash-table = (make-hash-table :test 'equal)
        for (event key) = (multiple-value-list (jzon:parse-next parser))
        until (eq event :end-object)
        do (handler-case
               (setf (gethash key hash-table)
                     (read-json-value parser (second json-type)))
             (null-value (condition)
               ;; Finish reading the object before passing the
               ;; error on, see the invariant above.
               (loop for (event) = (multiple-value-list
                                    (jzon:parse-next parser))
                     until (eq event :end-object)
                     do (skip-json-value parser (jzon:parse-next parser)))
               (error condition)))
        finally (return hash-table)))

(defun read-json-value (parser json-type &optional (end nil end-p))
  "Read the next value in PARSER as JSON-TYPE. When END is given, the
value is an array element, and END is returned at the end of the array."
  (flet ((read-streamed (begin-event reader)
           (multiple-value-bind (event value) (jzon:parse-next parser)
             (cond ((eq event begin-event) (values (funcall reader parser json-type)))
                   ((and end-p (eq event :end-array)) end)
                   ((eq event :value) (to-lisp-value value json-type))
                   (t (error 'json-type-error :json-type json-type))))))
    (cond ((json-class-p json-type)
           (read-streamed :begin-object #'read-json-object))
          ((homogeneous-type-p json-type '(:hash-table))
           (read-streamed :begin-object #'read-homogeneous-hash-table))
          ((homogeneous-type-p json-type '(:list :vector))
           (read-streamed :begin-array #'read-homogeneous-sequence))
          (t
           (let ((element (jzon:parse-next-element parser :eof-error-p (not end-p)
                                                           :eof-value end)))
             (if (and end-p (eq element end))
                 end
                 (to-lisp-value element json-type)))))))

(defgeneric json-to-clos (input class &rest initargs))

(defmethod json-to-clos ((input hash-table) class &rest initargs)
  (let* ((lisp-object (apply #'make-instance class initargs))
         (class-object (class-of lisp-object))
         (key-count 0))
    (loop for superclass in (closer-mop:class-precedence-list class-object)
          do (loop for slot in (closer-mop:class-direct-slots superclass)
                   when (typep slot 'json-serializable-slot)
                     do (awhen (json-key-name slot)
                          ;; Only the most specific slot with a key is set
                          (when (eq slot (find-json-slot class-object it))
                            (handler-case
                                (progn
                                  (setf (slot-value lisp-object
                                                    (closer-mop:slot-definition-name slot))
                                        (to-lisp-value (gethash it input 'null)
                                                       (json-type slot)))
                                  (incf key-count))
                              (null-value (condition)
                                (declare (ignore condition)) nil))))))
    (when (zerop key-count) (warn 'no-values-parsed
                                  :hash-table input
                                  :class-name class))
    (values lisp-object key-count)))

(defun parse-to-clos (input class initargs)
  "Read a JSON object from INPUT, which is anything JZON:MAKE-PARSER
accepts, into an instance of CLASS."
  (jzon:with-parser (parser input)
    (multiple-value-bind (event value) (jzon:parse-next parser)
      (declare (ignore value))
      (unless (eq event :begin-object)
        (error 'json-type-error :json-type class))
      (apply #'read-json-object parser class initargs))))

(defmethod json-to-clos ((input stream) class &rest initargs)
  (parse-to-clos input class initargs))

(defmethod json-to-clos ((input pathname) class &rest initargs)
  (parse-to-clos input class initargs))

(defmethod json-to-clos ((input string) class &rest initargs)
  (parse-to-clos input class initargs))
