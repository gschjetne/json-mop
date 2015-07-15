;; Copyright (c) 2015 Grim Schjetne
;;
;; This file is part of JSON-MOP
;;
;; JSON-MOP is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; JSON-MOP is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with JSON-MOP.  If not, see
;; <http://www.gnu.org/licenses/>.

(in-package #:json-mop)

(defclass json-serializable (closer-mop:standard-class) ())

(defmethod closer-mop:validate-superclass ((class json-serializable)
                                           (super closer-mop:standard-class))
  t)

(defmethod closer-mop:validate-superclass ((class standard-class)
                                           (super json-serializable))
  t)

(defclass json-serializable-slot (closer-mop:standard-direct-slot-definition)
  ((json-key :initarg :json-key
             :initform nil
             :reader json-key-name)
   (json-type :initarg :json-type
              :initform :any
              :reader json-type)))

(defmethod json-key-name ((slot closer-mop:standard-direct-slot-definition))
  (warn 'slot-not-serializable
        :slot-name (closer-mop:slot-definition-name slot)))

(defmethod closer-mop:direct-slot-definition-class ((class json-serializable)
                                                    &rest initargs)
  (declare (ignore class initargs))
  (find-class 'json-serializable-slot))

(defun to-json-value (value slot)
  (ecase (json-type slot)
    (:string (check-type value (or null string)) value)
    (:number (check-type value (or null number)) value)
    (:object value)
    (:vector (check-type value (or null (and sequence (not string)))) value)
    (:list (check-type value list) value)
    (:bool (check-type value boolean)
     (if value 'true 'false))))

(defgeneric to-lisp-value (value json-type)
  (:documentation
   "Turns a value passed by Yason into the appropriate
  Lisp type as specified by JSON-TYPE"))

(defmethod to-lisp-value ((value (eql :null)) json-type)
  "When the value is JSON null, signal NULL-VALUE error"
  (declare (ignore value))
  (error 'null-value :json-type json-type))

(defmethod to-lisp-value (value (json-type (eql :any)))
  "When the JSON type is :ANY, Pass the VALUE unchanged"
  (declare (ignore json-type)) value)

(defmethod to-lisp-value ((value string) (json-type (eql :string)))
  "Return the string VALUE"
  (declare (ignore json-type)) value)

(defmethod to-lisp-value ((value number) (json-type (eql :number)))
  "Return the number VALUE"
  (declare (ignore json-type)) value)

(defmethod to-lisp-value ((value hash-table) (json-type (eql :hash-table)))
  "Return the hash-table VALUE"
  (declare (ignore json-type)) value)

(defmethod to-lisp-value ((value vector) (json-type (eql :vector)))
  "Return the vector VALUE"
  (declare (ignore json-type)) value)

(defmethod to-lisp-value ((value vector) (json-type (eql :list)))
  "Return the list VALUE"
  (declare (ignore json-type)) (coerce value 'list))

(defmethod to-lisp-value (value (json-type (eql :bool)))
  "Return the boolean VALUE"
  (declare (ignore json-type))
  (ecase value (true t) (false nil)))

(defmethod to-lisp-value ((value vector) (json-type cons))
  "Return the homogenous sequence VALUE"
  (map (ecase (first json-type)
         (:vector 'vector)
         (:list 'list))
       (lambda (item)
         (handler-case (to-lisp-value item (second json-type))
           (null-value (condition)
             (declare (ignore condition))
             (restart-case (error 'null-in-homogenous-sequence
                                  :json-type json-type)
               (use-value (value)
                 :report "Specify a value to use in place of the null"
                 :interactive (lambda () (format *query-io* "Eval: ")
                                (list (eval (read *query-io*))))
                 value)))))
       value))

(defmethod to-lisp-value ((value hash-table) (json-type symbol))
  "Return the CLOS object VALUE"
  (json-to-clos value json-type))

(defgeneric json-to-clos (input class &rest initargs))

(defmethod json-to-clos ((input hash-table) class &rest initargs)
  (let ((lisp-object (apply #'make-instance class initargs))
        (key-count 0))
    (loop for slot in (closer-mop:class-direct-slots (find-class class))
          do (awhen (json-key-name slot)
               (handler-case
                   (progn
                     (setf (slot-value lisp-object
                                       (closer-mop:slot-definition-name slot))
                           (to-lisp-value (gethash it input :null)
                                          (json-type slot)))
                     (incf key-count))
                 (null-value (condition)
                   (declare (ignore condition)) nil))))
    (when (zerop key-count) (warn 'no-values-parsed
                                  :hash-table input
                                  :class-name class))
    (values lisp-object key-count)))

(defmethod json-to-clos ((input stream) class &rest initargs)
  (apply #'json-to-clos
         (parse input
                      :object-as :hash-table
                      :json-arrays-as-vectors t
                      :json-booleans-as-symbols t
                      :json-nulls-as-keyword t)
         class initargs))

(defmethod json-to-clos ((input pathname) class &rest initargs)
  (with-open-file (stream input)
    (apply #'json-to-clos stream class initargs)))

(defmethod json-to-clos ((input string) class &rest initargs)
  (apply #'json-to-clos (make-string-input-stream input) class initargs))

(defclass test-class ()
  ((string :initarg :string
           :json-type :string
           :json-key "string")
   (number :initarg :number
           :json-type :number
           :json-key "number")
   (hash :initarg :object
           :json-type :hash-table
           :json-key "hash")
   (vector :initarg :vector
           :json-type :vector
           :json-key "vector")
   (list :initarg :list
         :json-type :list
         :json-key "list")
   (bool :initarg :bool
         :json-type :bool
         :json-key "bool")
   (test-class :initarg :test-class
               :json-type test-class
               :json-key "test_object")
   (homogenous-list :initarg :homgenous-list
                    :json-type (:vector :number)
                    :json-key "homo"))
  (:metaclass json-serializable))
