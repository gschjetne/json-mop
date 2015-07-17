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

(defvar *encode-unbound-slots* nil)

(defgeneric to-json-value (value json-type)
  (:documentation
   "Turns a VALUE into a form appropriate for consumption by Yason"))

(defmethod to-json-value (value (json-type (eql :any)))
  "When the JSON type is :ANY, Pass the VALUE unchanged"
  (declare (ignore json-type)) value)

(defmethod to-json-value ((value null) (json-type keyword))
  (declare (ignore value))
  (error 'null-value :json-type json-type))

(defmethod to-json-value ((value string) (json-type (eql :string)))
  "Return the string VALUE"
  (declare (ignore json-type)) value)

(defmethod to-json-value ((value number) (json-type (eql :number)))
  "Return the number VALUE"
  (declare (ignore json-type)) value)

(defmethod to-json-value ((value hash-table) (json-type (eql :hash-table)))
  "Return the hash-table VALUE"
  (declare (ignore json-type)) value)

(defmethod to-json-value ((value vector) (json-type (eql :vector)))
  "Return the vector VALUE"
  (declare (ignore json-type)) value)

(defmethod to-json-value ((value list) (json-type (eql :list)))
  "Return the list VALUE"
  (declare (ignore json-type)) value)

(defmethod to-json-value ((value null) (json-type (eql :list)))
  "Return the empty list VALUE"
  (declare (ignore value json-type)) #())

(defmethod to-json-value (value (json-type (eql :bool)))
  "Return the boolean true"
  (declare (ignore value json-type)) 'true)

(defmethod to-json-value ((value null) (json-type (eql :bool)))
  "Return the boolean false"
  (declare (ignore value json-type)) 'false)

(defmethod to-json-value ((value sequence) (json-type cons))
  "Return the homogeneous sequence VALUE"
  (ecase (first json-type)
    (:list (check-type value list))
    (:vector (check-type value vector)))
  (make-instance 'homogeneous-sequence-intermediate-class
                 :values value
                 :sequence-json-type (first json-type)
                 :element-json-type (second json-type)))

(defclass homogeneous-sequence-intermediate-class ()
  ((values :initarg :values)
   (sequence-json-type :initarg :sequence-json-type)
   (element-json-type :initarg :element-json-type)))

(defmethod to-json-value (value (json-type symbol))
  (if (eql (class-of value) (find-class json-type))
      value
      (error 'json-type-error :json-type json-type)))

(defmethod encode ((sequence homogeneous-sequence-intermediate-class)
                   &optional (stream *standard-output*))
  (with-output (stream)
    (with-array ()
      (with-slots (values sequence-json-type element-json-type) sequence
        (map nil (lambda (element)
                   (handler-case
                       (encode-array-element (to-json-value element element-json-type))
                     (null-value (condition)
                       (declare (ignore condition))
                       (restart-case (error 'null-in-homogeneous-sequence
                                            :json-type (list sequence-json-type
                                                             element-json-type))
                         (use-value (value)
                           :report "Specify a value to use in place of the null"
                           :interactive read-eval-query
                           (encode-array-element value))))))
             values))))
  sequence)

(defmethod encode ((object json-serializable-mixin)
                   &optional (stream *standard-output*))
  (with-output (stream)
    (with-object ()
      (loop for slot in (closer-mop:class-direct-slots (class-of object))
            do (awhen (json-key-name slot)
                 (handler-case
                     (encode-object-element
                      it
                      (to-json-value
                       (slot-value object (closer-mop:slot-definition-name slot))
                       (json-type slot)))
                   (unbound-slot (condition)
                     (declare (ignore condition))
                     (when *encode-unbound-slots*
                       (encode-object-element it nil))))))))
  object)
