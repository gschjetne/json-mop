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

(define-condition slot-not-serializable (warning)
  ((slot-name :initarg :slot-name
              :reader slot-name))
  (:report (lambda (condition stream)
             (format stream "Slot ~A has no JSON metadata associated with it."
                     (slot-name condition)))))

;; TODO: inherit TYPE-ERROR and change all occurences to specify
;; :expected-type, likewise change relevant occurrences of TYPE-ERROR
;; to JSON-TYPE-ERROR
(define-condition json-type-error (error)
  ((json-type :initarg :json-type
              :reader json-type)))

(define-condition null-value (json-type-error) ())

(define-condition null-in-homogenous-sequence (json-type-error) ()
  (:report (lambda (condition stream)
             (format stream "null encountered in a homogenous sequence of type ~S"
                     (json-type condition)))))

(define-condition no-values-parsed (warning)
  ((hash-table :initarg :hash-table
               :reader no-values-hash-table)
   (class-name :initarg :class-name
               :reader no-values-class))
  (:report (lambda (condition stream)
             (format stream "No keys corresponding to slots in ~A found in ~A"
                     (no-values-class condition)
                     (no-values-hash-table condition)))))

(defun read-eval-query ()
  (format *query-io* "Eval: ")
  (list (eval (read *query-io*))))
