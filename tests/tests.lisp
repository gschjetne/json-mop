;; Copyright (c) 2016 Grim Schjetne
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

(in-package #:json-mop-tests)

(defclass test-class ()
  ((string :initarg :string
           :reader get-string
           :json-type :string
           :json-key "str")
   (number :initarg :number
           :reader get-number
           :json-type :number
           :json-key "num")
   (integer :initarg :integer
	    :reader get-integer
	    :json-type :integer
	    :json-key "int")
   (hash :initarg :hash-table
         :reader get-hash-table
         :json-type :hash-table
         :json-key "hash")
   (any-hash :initarg :any-hash-table
             :reader get-any-hash-table
             :json-type :any
             :json-key "anyHash")
   (obj-hash :initarg :obj-hash-table
             :reader get-obj-hash-table
             :json-type (:hash-table (:vector :string))
             :json-key "objHash")
   (vector :initarg :vector
           :reader get-vector
           :json-type :vector
           :json-key "vect")
   (list :initarg :list
         :reader get-list
         :json-type :list
         :json-key "list")
   (bool :initarg :bool
         :reader get-bool
         :json-type :bool
         :json-key "bool")
   (object :initarg :object
           :reader get-object
           :json-type test-class
           :json-key "obj")
   (any :initarg :any
        :reader get-any
        :json-type :any
        :json-key "any"))
  (:metaclass json-serializable-class))

;;; as per https://github.com/gschjetne/json-mop/issues/1
(defclass parent ()
  ((foo :accessor foo :initarg :foo
          :initform "Foo"
          :json-key "foo"))
  (:metaclass json-serializable-class))

(defclass child (parent)
    ((bar :accessor bar :initarg :bar
      :json-key "bar"))
  (:metaclass json-serializable-class))

(defun json-string (object)
  (with-output-to-string (s)
    (encode object s)))

(defun obj-rt (object)
  (json-to-clos (json-string object)
                (class-name (class-of object))))

(defun gen-vector (&key
                     (length (gen-integer :min 0 :max 10))
                     (elements (gen-integer :min -10 :max 10)))
  (lambda ()
    (let* ((l (funcall length))
           (vector (make-array l)))
      (loop for i from 0 to (1- l) do
            (setf (aref vector i) (funcall elements)))
      vector)))

(defun gen-bool ()
  (lambda () (zerop (random 2))))

(defun gen-any (&key (choices (list (gen-string)
                                    (gen-float)
                                    (gen-vector))))
  (lambda ()
    (funcall (nth (random (length choices))
                  choices))))

(defun gen-hash-table (&key
                         (length (gen-integer :min 0 :max 10))
                         (keys (gen-string))
                         (elements (gen-integer)))
  (lambda ()
    (let ((hash-table (make-hash-table :test 'equal)))
      (loop repeat (funcall length)
            do (setf (gethash (funcall keys) hash-table)
                     (funcall elements)))
      hash-table)))

(defun gen-object (&key
                     (string (gen-string))
                     (number (gen-float))
		     (integer (gen-integer))
                     (hash-table (gen-hash-table))
		     (obj-hash-table
		      (gen-hash-table
		       :elements (gen-vector :elements (gen-string))))
                     (vector (gen-vector))
                     (list (gen-list))
                     (bool (gen-bool))
                     (object (lambda () (make-instance
                                    'test-class
                                    :number (funcall (gen-integer)))))
                     (any (gen-any)))
  (lambda ()
    (make-instance 'test-class
                   :string (funcall string)
                   :number (funcall number)
		   :integer (funcall integer)
                   :hash-table (funcall hash-table)
                   :any-hash-table (funcall hash-table)
                   :obj-hash-table (funcall obj-hash-table)
                   :vector (funcall vector)
                   :list (funcall list)
                   :bool (funcall bool)
                   :object (funcall object)
                   :any (funcall any))))

(def-suite test-all)
