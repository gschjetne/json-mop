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

(def-suite inheritance
  :in test-all
  :description "Test encoding and decoding across class hierarchies")

(in-suite inheritance)

(defclass grandparent ()
  ((a :initarg :a :reader a :json-type :integer :json-key "a"))
  (:metaclass json-serializable-class))

(defclass middle (grandparent)
  ((b :initarg :b :reader b :json-type :integer :json-key "b"))
  (:metaclass json-serializable-class))

(defclass grandchild (middle)
  ((c :initarg :c :reader c :json-type :integer :json-key "c"))
  (:metaclass json-serializable-class))

(test decode-inherited-slots
  "Slots from every ancestor are populated when decoding (issue #19)."
  (let ((obj (json-to-clos "{\"a\": 1, \"b\": 2, \"c\": 3}" 'grandchild)))
    (is (= 1 (a obj)))
    (is (= 2 (b obj)))
    (is (= 3 (c obj)))))

(test round-trip-inherited-slots
  (let* ((obj (make-instance 'grandchild :a 1 :b 2 :c 3))
         (rt (obj-rt obj)))
    (is (= (a obj) (a rt)))
    (is (= (b obj) (b rt)))
    (is (= (c obj) (c rt)))))
