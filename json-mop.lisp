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
                                           (super closer-mop:standard-class)) t)

(defmethod closer-mop:validate-superclass ((class standard-class)
                                           (super json-serializable)) t)

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
