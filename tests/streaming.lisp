;; Copyright (c) 2026 Grim Schjetne
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

(def-suite streaming
  :in test-all
  :description "Test decoding JSON text directly into slots")

(in-suite streaming)

(defclass point ()
  ((x :initarg :x :reader x :json-type :integer :json-key "x")
   (y :initarg :y :reader y :json-type :integer :json-key "y"))
  (:metaclass json-serializable-class))

(defclass shape ()
  ((name :initarg :name :reader name :json-type :string :json-key "name")
   (points :initarg :points :reader points :json-type (:list point)
           :json-key "points")
   (named-points :initarg :named-points :reader named-points
                 :json-type (:hash-table point) :json-key "namedPoints")
   (weights :initarg :weights :reader weights
            :json-type (:hash-table :integer) :json-key "weights")
   (tags :initarg :tags :reader tags :json-type (:vector :string)
         :json-key "tags"))
  (:metaclass json-serializable-class))

(test skip-unknown-keys
  "Values of keys without a slot are skipped, however deeply nested."
  (let ((shape (json-to-clos "{\"junk\": {\"a\": [1, {\"b\": []}, null]},
                               \"name\": \"square\",
                               \"more\": [[], {}]}"
                             'shape)))
    (is (string= "square" (name shape)))))

(test nested-objects
  (let ((shape (json-to-clos "{\"points\": [{\"x\": 1, \"y\": 2}, {\"x\": 3}],
                               \"namedPoints\": {\"origin\": {\"x\": 0, \"y\": 0}},
                               \"tags\": [\"a\", \"b\"]}"
                             'shape)))
    (is (equal '(1 3) (mapcar #'x (points shape))))
    (is (not (slot-boundp (second (points shape)) 'y)))
    (is (= 0 (y (gethash "origin" (named-points shape)))))
    (is (equalp #("a" "b") (tags shape)))))

(test use-value-keeps-reading
  "Reading carries on after a null is replaced in a homogeneous sequence."
  (let ((shape (handler-bind ((null-in-homogeneous-sequence
                                (lambda (c) (use-value (make-instance 'point) c))))
                 (json-to-clos "{\"points\": [null, {\"x\": 1}], \"name\": \"n\"}"
                               'shape))))
    (is (= 2 (length (points shape))))
    (is (= 1 (x (second (points shape)))))
    (is (string= "n" (name shape)))))

(test null-in-hash-table-keeps-reading
  "A null in a homogeneous hash table leaves the slot unbound, and the
following keys are still read."
  (let ((shape (json-to-clos "{\"weights\": {\"a\": 1, \"b\": null, \"c\": [2]},
                               \"name\": \"n\"}"
                             'shape)))
    (is (not (slot-boundp shape 'weights)))
    (is (string= "n" (name shape)))))

(test not-an-object
  (signals json-type-error (json-to-clos "[1, 2]" 'point))
  (signals json-type-error (json-to-clos "null" 'point)))

(test no-values-parsed
  (signals no-values-parsed (json-to-clos "{}" 'point))
  (signals no-values-parsed (json-to-clos "{\"z\": 1}" 'point)))

(test key-count
  (is (= 2 (nth-value 1 (json-to-clos "{\"x\": 1, \"y\": 2, \"z\": 3}" 'point)))))

(test initargs
  "Values from the JSON override the initargs."
  (let ((point (json-to-clos "{\"x\": 1}" 'point :x 5 :y 6)))
    (is (= 1 (x point)))
    (is (= 6 (y point)))))

(test inputs
  "Streams and hash tables parsed by jzon are accepted as input."
  (let ((json "{\"x\": 1, \"y\": 2}"))
    (is (= 2 (y (with-input-from-string (s json) (json-to-clos s 'point)))))
    (is (= 2 (y (json-to-clos (com.inuoe.jzon:parse json) 'point))))))

(test jzon-interop
  "Objects can be written with jzon directly."
  (is (string= "{\"x\":1,\"y\":2}"
               (com.inuoe.jzon:stringify (make-instance 'point :x 1 :y 2)))))

(test duplicate-keys
  "The last of duplicate keys wins."
  (is (= 2 (x (json-to-clos "{\"x\": 1, \"x\": 2}" 'point)))))

(defclass keyed-parent ()
  ((parent-slot :initarg :parent-slot :json-type :integer :json-key "k"))
  (:metaclass json-serializable-class))

(defclass keyed-child (keyed-parent)
  ((child-slot :initarg :child-slot :reader child-slot
               :json-type :string :json-key "k"))
  (:metaclass json-serializable-class))

(test same-key-in-subclass
  "A key shared with a superclass sets only the most specific slot."
  (dolist (input (list "{\"k\": \"s\"}"
                       (com.inuoe.jzon:parse "{\"k\": \"s\"}")))
    (let ((child (json-to-clos input 'keyed-child)))
      (is (string= "s" (child-slot child)))
      (is (not (slot-boundp child 'parent-slot))))))

(test pathname-input
  (let ((path (uiop:with-temporary-file (:stream s :pathname p :keep t)
                (write-string "{\"x\": 1, \"y\": 2}" s)
                p)))
    (unwind-protect (is (= 2 (y (json-to-clos path 'point))))
      (delete-file path))))

(test truncated-input
  "Truncated input signals a parse error rather than looping."
  (signals com.inuoe.jzon:json-parse-error
    (json-to-clos "{\"points\": [{\"x\": 1" 'shape))
  (signals com.inuoe.jzon:json-parse-error
    (json-to-clos "{\"junk\": [[" 'point)))

(test wrong-container
  (signals json-type-error (json-to-clos "{\"points\": {}}" 'shape))
  (signals json-type-error (json-to-clos "{\"weights\": []}" 'shape))
  (signals json-type-error (json-to-clos "{\"points\": [[]]}" 'shape)))
