(in-package :cl-cgen)


(defun gencfilebody (tags name body)
  `((defun ,(gen-sym :make :file name)  ()
      '(progn ,@body))
    (tag-assoc ',name #',(gen-sym :make :file  name))
    (tag-assoc :file ',name)
    (defun gen-sym (&rest args)
      (read-from-string (format nil "~{~a~^-~}" args)))

    (defun to-c-name (str)
      (if (and (listp str) (eq (first str) :preserve))
          (symbol-name (second str))
          (if (equalp str 'null)
              "NULL"
              (string-downcase
               (substitute #\_ #\- (symbol-name str))))))
    ,@(loop :for x :in tags
            :collect `(tag-assoc ,x ',name))))

(defmacro defcfile (tags name &body body)
  `(progn
     ,@(gencfilebody tags name body)))
