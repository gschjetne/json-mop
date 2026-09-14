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

(def-suite null-handling
  :in test-all
  :description "Test handling of JSON null and NIL in typed slots")

(in-suite null-handling)

(defclass int-list-box ()
  ((items :initarg :items
          :reader items
          :json-type (:list :integer)
          :json-key "items"))
  (:metaclass json-serializable-class))

(defclass optional-box ()
  ((foo :initarg :foo
        :reader foo
        :json-type :string
        :json-key "foo"))
  (:metaclass json-serializable-class))

(test decode-null-in-homogeneous-sequence
  "Decoding null inside a typed sequence signals the exported condition."
  (signals null-in-homogeneous-sequence
    (json-to-clos "{\"items\": [1, null, 3]}" 'int-list-box)))

(test decode-null-in-homogeneous-sequence-use-value
  "The USE-VALUE restart substitutes a value for the null."
  (let ((box (handler-bind ((null-in-homogeneous-sequence
                              (lambda (c) (use-value 0 c))))
               (json-to-clos "{\"items\": [1, null, 3]}" 'int-list-box))))
    (is (equal '(1 0 3) (items box)))))

(test encode-nil-in-homogeneous-sequence
  "Encoding NIL inside a typed sequence signals the exported condition."
  (signals null-in-homogeneous-sequence
    (json-string (make-instance 'int-list-box :items '(1 nil 3)))))

(test decode-null-slot-is-unbound
  "A null slot value is left unbound."
  (let ((box (json-to-clos "{\"foo\": null}" 'optional-box)))
    (is (not (slot-boundp box 'foo)))))

(test encode-unbound-slots
  "Unbound slots are omitted by default and encoded as null when
*ENCODE-UNBOUND-SLOTS* is true."
  (let ((box (make-instance 'optional-box)))
    (is-false (nth-value 1 (gethash "foo" (yason:parse (json-string box)))))
    (let ((*encode-unbound-slots* t))
      (is-true (nth-value 1 (gethash "foo" (yason:parse (json-string box))))))))
