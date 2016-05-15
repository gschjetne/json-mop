;; Copyright (c) 2016 Grim Schjetne
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
   (hash :initarg :hash-table
         :reader get-hash-table
         :json-type :hash-table
         :json-key "hash")
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
           :json-key "obj"))
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

(defun gen-object (&key
                     (string (gen-string))
                     (number (gen-float))
                     (hash-table (lambda () (make-hash-table)))
                     (vector (gen-vector))
                     (list (gen-list))
                     (bool (gen-bool))
                     (object (lambda () (make-instance
                                    'test-class
                                    :number (funcall (gen-integer))))))
  (lambda ()
    (make-instance 'test-class
                   :string (funcall string)
                   :number (funcall number)
                   :hash-table (funcall hash-table)
                   :vector (funcall vector)
                   :list (funcall list)
                   :bool (funcall bool)
                   :object (funcall object))))

(def-suite test-all)
