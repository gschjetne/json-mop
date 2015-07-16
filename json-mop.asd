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

(asdf:defsystem #:json-mop
  :description "A metaclass for bridging CLOS and JSON"
  :author "Grim Schjetne"
  :license "LGPLv3+"
  :depends-on (#:closer-mop
               #:yason
               #:anaphora)
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "json-mop")
               (:file "to-lisp")
               (:file "to-json")))
