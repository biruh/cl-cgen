(in-package :cl-cgen)


(defun generate-header (name body &key (hash nil))
  (let ((pname (string-replace-all "." (to-c-name name) "_")))
    (format nil "#ifndef ~a~%#define ~a~%~a~a~%#endif" pname pname
            (if hash
                (format nil "#define ~a_HASH \"~a\" ~%" pname hash)
                "")
            body)))


(defun compile-and-build (task)
  (setf *next-lib-id* (+ *next-lib-id* 1))
  (format t "~%======COMPILING (~a)=======~%" *next-lib-id*)
  (sb-ext:run-program  "make"  (list task)
                       :search "/bin/bash"
                       :directory *client-build-directory*
                       :status-hook (lambda (a)
                                      (if (not (zerop  (sb-ext:process-exit-code a)))
                                          (error (make-condition 'simple-error
                                                                 :format-control "client: ~a compilation error ~a~%"
                                                                 :format-arguments (list task a ))))
                                      )
                       :input nil
                       :wait t
                       :output *standard-output*)
  (sb-ext:run-program "cp" (list "liblib.so.1.0.1" (format nil "liblib~a.so" *next-lib-id*))
                      :search "/bin/bash"
                      :directory *client-build-directory*
                      :input nil
                      :wait t
                      :output *standard-output*)

  )





(defun format-c-file (filename)
  ;; (format t "~%clang-format ~a" filename)
  (sb-ext:run-program  "clang-format"  (list "-i" filename)
                       :search "/bin/bash"
                       :directory *client-directory*
                       :status-hook (lambda (a)
                                      (if (not (zerop (sb-ext:process-exit-code a)))
                                          (error (make-condition 'simple-error
                                                                 :format-control "error formating source code: ~a~%"
                                                                 :format-arguments (list a ))))
                                      )
                       :input nil
                       :wait t
                       :output *standard-output*))
