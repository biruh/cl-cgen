(in-package :cl-cgen)


(defun gencfunbody (template name args return body)
  (let ((form `((defun ,(gen-sym :make :func name)  ()
                  '(:function ,name ,args ,return
                    (progn
                      ,@body)))
                (defun ,(gen-sym :make :func :defn name) ()
                  '(:function ,name ,args ,return)))))
    (if template
        (replace-single-template-var form template)
        form)))


(defun struct-maker-maker (tags name doc-string slots)
  `(progn
     (defun ,(gen-sym :make :struct name :documentation) ()
       ,doc-string)
     (defun ,(gen-sym :make :struct name) ()
       '(progn
         (:comment ,doc-string)
         (:typedef
          (:struct ,name
           ,@slots))))
     (g>set :struct ',name #',(gen-sym :make :struct name))
     (g>join :tag :struct ',name)
     ,@(loop :for x :in tags
             :collect `(g>join :tag ,x ',name)))
  )



(defmacro defcfun (name args return &body body)
  `(progn
     ,@(if (listp name)
           (loop :for fb :in (rest name)
                 :append (gencfunbody fb (gen-sym (first name) fb) args return body))
           (gencfunbody nil name args return body))))


(defmacro defcstruct (tags &rest defns)
  `(progn
     ,@(loop :for defn :in defns
             :append (destructuring-bind (name &body body) defn
                       (struct-maker-maker tags name "" body)))))


(defmacro defcenum (tags &rest defns)
  `(progn
     ,@(loop :for defn :in defns
             :append (destructuring-bind (name &body body) defn
                       `((defun ,(gen-sym :make :enum name) ()
                           '(:typedef
                             (:enum ,name
                              ,@body)))
                         (g>set :enum ,name #',(gen-sym :make :enum name))
                         (g>join :tag :enum ,name)
                         ,@(loop :for x :in tags
                                 :collect `(g>join :tag ,x ,name))
                         )))))
