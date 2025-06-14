(defpackage cl-cgen/tests/main
  (:use :cl
        :cl-cgen
        :rove))
(in-package :cl-cgen/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-cgen)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
