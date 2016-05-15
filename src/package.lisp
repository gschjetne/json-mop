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

(defpackage #:json-mop
  (:use #:cl)
  (:import-from #:yason
                #:true
                #:false
                #:parse
                #:encode
                #:encode-array-element
                #:encode-object-element
                #:with-output
                #:with-array
                #:with-object)
  (:import-from #:anaphora
                #:awhen
                #:it)
  (:export #:json-serializable
           #:to-lisp-value
           #:to-json-value
           #:json-to-clos
           ;; Re-export yason:encode
           #:encode
           ;; Conditions
           #:slot-not-serializable
           #:slot-name
           #:json-type-error
           #:json-type
           #:null-value
           #:null-in-homogenous-sequence
           #:no-values-parsed
           #:no-values-hash-table
           #:no-values-class))
