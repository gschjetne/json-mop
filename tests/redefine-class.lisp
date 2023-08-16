;; Copyright (c) 2023 Grim Schjetne
;; See COPYING for license information

(in-package #:json-mop-tests)

(def-suite redefine-class
  :in test-all
  :description "Test redefining serializable class")

(in-suite redefine-class)

(test redefine-class
  (defclass redefined-class ()
    ()
    (:metaclass json-serializable-class))

  (defclass redefined-class ()
    ((foo :json-key "foo"
          :json-type :number
          :initform 1))
    (:metaclass json-serializable-class))

  (let* ((instance (make-instance 'redefined-class))
         (parsed (yason:parse (json-string instance))))
    (is (= 1 (gethash "foo" parsed)))))
