(in-package :cl-cgen)


(defun gen-sym (&rest args)
  (read-from-string (format nil "~{~a~^-~}" args)))

(defun to-c-name (str)
  (if (and (listp str) (eq (first str) :preserve))
      (symbol-name (second str))
      (if (equalp str 'null)
          "NULL"
          (string-downcase
           (substitute #\_ #\- (symbol-name str))))))
