(in-package :cl-cgen)


(defcfun add-two-numbers ((:int a) (:int b)) :int
  (:return (+ a b)))


(defcfun (gen-two-numbers ()) ((:int a) (:int b)) :int
  (:return (+ a b)))


(make-func-add-two-numbers)

(genc
 (make-func-add-two-numbers))

(assert
 (equal
  (make-func-add-two-numbers)
  '(:FUNCTION ADD-TWO-NUMBERS ((:INT A) (:INT B)) :INT
    (progn (:return (+ A B))))))



"int add_two_numbers(int a, int b)
{
+(a, b);
}
"

(format t "~a~%" (genc (make-func-add-two-numbers)))
