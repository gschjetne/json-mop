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
              :initform :object
              :reader json-type)))

(defmethod closer-mop:direct-slot-definition-class ((class json-serializable)
                                                    &rest initargs)
  (declare (ignore class initargs))
  (find-class 'json-serializable-slot))

(define-condition null-value (error) ())

(defun to-json-value (value slot)
  (ecase (json-type slot)
    (:string (check-type value (or null string)) value)
    (:number (check-type value (or null number)) value)
    (:object value)
    (:vector (check-type value (or null (and sequence (not string)))) value)
    (:list (check-type value list) value)
    (:bool (check-type value boolean)
     (if value 'true 'false))))

(defun to-lisp-value (value slot)
  (when (eql value :null)
    (error 'null-value))
  (let ((json-type (json-type slot)))
    (etypecase json-type
      (keyword
       (ecase json-type
         (:string (check-type value string) value)
         (:number (check-type value number) value)
         (:object value)
         (:vector (check-type value vector) value)
         (:list (coerce value 'list))
         (:bool (ecase value
                  (true t)
                  (false nil)))))
      (symbol (hash-to-object value json-type)))))

(define-condition no-values-parsed (warning) ())

(defun hash-to-object (hash class &rest initargs)
  (check-type hash hash-table)
  (let ((lisp-object (apply #'make-instance class initargs))
        (key-count 0))
    (loop for slot in (closer-mop:class-direct-slots (find-class class))
          do (awhen (json-key-name slot)
               (handler-case
                   (progn
                     (setf (slot-value lisp-object
                                       (closer-mop:slot-definition-name slot))
                           (to-lisp-value (gethash it hash :null) slot))
                     (incf key-count))
                 (null-value (condition)
                   (declare (ignore condition)) nil))))
    (when (zerop key-count) (warn 'no-values-parsed))
    (values lisp-object key-count)))

(defun parse-as (input class &rest initargs)
  (apply #'hash-to-object
         (parse input
                      :object-as :hash-table
                      :json-arrays-as-vectors t
                      :json-booleans-as-symbols t
                      :json-nulls-as-keyword t)
         class initargs))

(defclass test-class ()
  ((string :initarg :string
           :json-type :string
           :json-key "string")
   (number :initarg :number
           :json-type :number
           :json-key "number")
   (object :initarg :object
           :json-type :object
           :json-key "object")
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
               :json-key "test_object"))
  (:metaclass json-serializable))
