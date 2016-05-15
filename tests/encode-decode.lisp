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

(def-suite encode-decode
  :in test-all
  :description "Test encoding and decoding slots")

(in-suite encode-decode)

(test string
  (for-all ((obj (gen-object)))
    (is (equal (get-string obj)
               (get-string (obj-rt obj))))))

(test number
  (for-all ((obj (gen-object)))
    (is (= (get-number obj)
           (get-number (obj-rt obj))))))

;; TODO: test hash table

(test vector
  (for-all ((obj (gen-object)))
    (is (equalp (get-vector obj)
                (get-vector (obj-rt obj))))))

(test list
  (for-all ((obj (gen-object)))
    (is (equal (get-list obj)
               (get-list (obj-rt obj))))))

(test bool
  (for-all ((obj (gen-object)))
    (is (eql (get-bool obj)
             (get-bool (obj-rt obj))))))

(test object
  (for-all ((obj (gen-object)))
    (is (= (get-number (get-object obj))
           (get-number (get-object (obj-rt obj)))))))
