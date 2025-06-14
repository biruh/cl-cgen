(in-package :cl-cgen)


(defcfile (:test) main.c
  (:include stdio.h))


(assert
 (equal
  (make-file-main.c)
  '(PROGN (:INCLUDE STDIO.H))))

(assert
 (string-equal
  (genc (make-file-main.c))
  (format nil "#include \"stdio.h\"~%")))
