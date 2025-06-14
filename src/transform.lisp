(in-package :cl-cgen)

(defun match-type-names (form)
  (declare (ignore form)))

(defun match-type (form)
  (trivia:match form
    ((list :ptr type-name)
     (format nil "~a *" (to-c-name type-name)))
    ((list :embed str)
     (format nil "~a" str))
    (type-name
     (format nil "~a " (to-c-name type-name)))
    (_ (error "Unknown type declaration ~a" (list form)))))

(defun match-name-value (form)
  (trivia:match form
    ((list name value)
     (format nil "~a = ~a" (to-c-name name) value))
    (_ (error "Unknown declaration name: ~a" (list form)))))

(defun render-attributes (attributes)
  (if attributes
      (format nil "~{~a~^ ~} "
              (loop :for a :in attributes
                    :collect (to-c-name a)))
      ""))

(defun match-type-name (form &key (attributes nil))
  (trivia:match form
    ((list (list :ptr type) name)
     (format nil "~a~a *~a"
             (render-attributes attributes)
             (to-c-name type)
             (match-expr name)))
    ((list (list :ptr :struct type) name)
     (format nil "struct ~a *~a" (to-c-name type) (to-c-name name)))
    ;; ((list type (list :fptr name args))
    ;;  (format nil "~a (*~a)(~a)"
    ;;          (to-c-name type)
    ;;          (match-expr name)
    ;;          (match-type-name-args args)))
    ((list* :union a)
     (progn
       (format nil "union {~%~a}"
               (match-type-name-decls a)
               )))
    ((list type name)
     (format nil "~a~a ~a"
             (render-attributes attributes)
             (to-c-name type)
             (match-expr name)))
    (name
     (format nil "~a"  (to-c-name name)))
    (_ (error "Unknown declaration name: ~a" (list form)))
    ))

(defun match-type-name-args (form)
  (format nil "~{~a~^, ~}"
          (loop :for fr :in form
                :when (eq (length fr) 2)
                  :collect (match-type-name fr)
                :else
                  :collect (match-type-name (last fr 2) :attributes (butlast fr 2)))))


(defun match-type-name-decls (form)
  (format nil "~{~a;~^~%~}~%"
          (loop :for fr :in form
                :collect (match-type-name fr))))

(defun match-name-value-decls (form)
  (format nil "~{~a;~^~%~}~%"
          (loop :for fr :in form
                :collect (match-name-value fr))))

(defparameter *bool-funcs* (serapeum:dict
                            'and "&&"
                            'or "||"))

(defun is-infix-bool (sym)
  (multiple-value-bind (v f) (gethash sym *bool-funcs*)
    (declare (ignore v))
    f))




(defun match-expr (form &key (no-fn-call nil) (no-safe-bracket nil))
  (trivia:match form
    ((list :embed str)
     (format nil "~a~%" str))
    ((list :preserve sym)
     (format nil "~a" (symbol-name sym)))
    ((list :cast target value)
     (format nil "((~a)~a)" (match-type target) (match-expr value)))
    ((list :dref ob)
     (format nil "*~a" (match-expr ob)))
    ((list :addr-of ob)
     (format nil "&~a" (match-expr ob)))
    ((list* :array ob)
     (format nil "{~{~a~^,~}}"
             (loop :for a :in ob
                   :collect (match-expr a))))
    ((list :array-of ob)
     (format nil "~a[]" (match-expr ob)))
    ((list :array-of ob index)
     (format nil "~a[~a]" (match-expr ob) (match-expr index)))
    ((list :pref ob attr)
     (format nil "~a->~a" (match-expr ob) (to-c-name attr)))
    ((list :oref ob attr)
     (format nil "~a.~a" (match-expr ob) (to-c-name attr)))
    ((list :aref ob attr)
     (format nil "~a[~a]" (match-expr ob) (match-expr attr)))
    ((list* :funcall func args)
     (format nil "~a(~{~a~^, ~})"
             (match-expr func)
             (loop :for sts :in args
                   :collect (match-expr sts))))
    ((list (list :ptr type) name)
     (format nil "~a *~a" (to-c-name type) (to-c-name name)))
    ((list (list :ptr :struct type) name)
     (format nil "struct ~a *~a" (to-c-name type) (to-c-name name)))
    ((list :fptr name args)
     (format nil "(*~a)(~a)"
             (to-c-name name)
             (match-type-name-args args)
             ))
    ((list :fptr retrn name args)
     (format nil "~a(*~a)(~a)"
             (match-type retrn)
             (to-c-name name)
             (match-type-name-args args)
             ))
    ((list :obj 0)
     (format nil "{0}"))
    ((list* :obj terms)
     (format nil "{~{ ~a~^,~}}"
             (loop :for term :in terms
                   :when term
                     :collect (format nil ".~a=~a"
                                      (match-expr (first term) :no-fn-call t)
                                      (match-expr (second term))))))
    ((list* :init-struct struct terms)
     (format nil "(~a){~{ ~a~^,~}}"
             (to-c-name struct)
             (loop :for term :in terms
                   :collect (format nil ".~a=~a"
                                    (to-c-name (first term))
                                    (match-expr (second term))))))
    ((list* :block body)
     (format nil "{~a}" (match-statement body)))
    ((trivia:guard (list func-name arg1 arg2)
                   (and (null no-fn-call)
                        (or
                         (eq func-name '!=)
                         (eq func-name '==)
                         (eq func-name '<=)
                         (eq func-name '>=)
                         (eq func-name '>)
                         (eq func-name '<)
                         (eq func-name '%)
                         (eq func-name '*)
                         (eq func-name '/)
                         (eq func-name '+)
                         (eq func-name '-))))
     (if no-safe-bracket
         (format nil "~a ~a ~a"
                 (match-expr arg1) func-name (match-expr arg2))
         (format nil "(~a ~a ~a)"
                 (match-expr arg1) func-name (match-expr arg2))))
    ((trivia:guard (list bools arg1 arg2) (and (null no-fn-call)
                                               (is-infix-bool bools)))
     (format nil "~a ~a ~a"
             (match-expr arg1)
             (gethash bools *bool-funcs*)
             (match-expr arg2) ))
    ((trivia:guard (list* func-name args) (null no-fn-call))
     (format nil "~a(~{~a~^, ~})"
             (to-c-name func-name)
             (loop :for sts :in args
                   :collect (match-expr sts)
                   )))
    ((trivia:guard n (numberp n)) (format nil "~a" n))
    ((trivia:guard s (stringp s)) (format nil "\"~a\"" s))
    ((trivia:guard a (atom a)) (format nil "~a" (to-c-name a)))
    ((trivia:guard lst (listp lst))
     (format nil "~{~a ~}"
             (loop :for x :in lst
                   :collect (match-expr x))))
    (n (format nil "~a" (to-c-name n)))
    (_ (format nil "match in fun fell through: ~a" form))
    ))



(defun match-statement (form)
  (trivia:match form
    ((trivia:guard (list* pgn body) (eql 'progn pgn))
     (format nil "~{~a~^~0,1t~}"
             (loop :for sts :in body
                   :collect (match-statement sts))))
    ((list :set identifier)
     (format nil "~a;~%"
             (match-expr identifier :no-fn-call t)))
    ((list :set identifier value )
     (format nil "~a = ~a;~%"
             (match-expr identifier :no-fn-call t)
             (match-expr value)))
    ((list :embed str)
     (format nil "~a~%" str))
    ((list :comment str)
     (format nil "//~a~%" str))
    ((list :return exp)
     (format nil "return ~a;" (match-expr exp)))
    ((list :break)
     (format nil "break;"))
    ((list* :block body)
     (format nil "{~a ~%}" (match-statement body)))
    ((list* 'when pred stmts)
     (format nil "if (~a) {~a}"
             (match-expr pred :no-safe-bracket t)
             (match-statement stmts)))
    ((list 'if pred stmt)
     (format nil "if (~a) ~a"
             (match-expr pred :no-safe-bracket t)
             (match-statement stmt)))
    ((list 'if pred stmt altstmt)
     (format nil "if (~a) ~a else ~a"
             (match-expr pred :no-safe-bracket t)
             (match-statement stmt)
             (match-statement altstmt)))
    ((list :goto label)
     (format nil "goto ~a;" (to-c-name label)))
    ((list 'loop :for var1 :below limit :do stmts)
     (format nil "for (size_t ~a = 0;~a < ~a; ~a++){~a}"
             (to-c-name var1)
             (to-c-name var1)
             (match-expr limit)
             (to-c-name var1)
             (match-statement stmts)))
    ((list :label label)
     (format nil "~a:" (to-c-name label)))
    ((list* :switch target cases)
     (format nil "switch (~a)~%{~{~1,1t~a~^~%~}~%}"
             (match-expr target)
             (loop :for case :in cases
                   :when (eq (first case) t)
                     :collect (format nil "default: ~a"
                                      (match-statement (second case)))
                   :else
                     :collect (format nil "case ~a:~%~a"
                                      (match-expr (first case))
                                      (match-statement (second case))))))
    ((list :apply func args)
     (format nil "~a(~{~a~^, ~});~%"
             (match-expr func)
             (loop :for sts :in args
                   :collect (match-expr sts))))
    ((list* :funcall func args)
     (format nil "~a(~{~a~^, ~});~%"
             (match-expr func)
             (loop :for sts :in args
                   :collect (match-expr sts))))
    ((trivia:guard (list* func-name args) (symbolp func-name))
     (format nil "~a(~{~a~^, ~});~%"
             (to-c-name func-name)
             (loop :for sts :in args
                   :collect (match-expr sts)
                   )))
    ((list* body)
     (format nil "~{~a~}"
             (loop :for x :in body
                   :collect (match-statement x))))
    (_ (format nil "match in fun fell through: ~a" form))))


(defun match-top-level (form)
  (trivia:match form
    ((trivia:guard (list* pgn body) (eql 'progn pgn))
     (format nil "~{~a~}"
             (loop :for sts :in body
                   :collect (match-top-level sts))))
    ((list :comment comment)
     (format nil "~%/*~a*/~%" comment))
    ((list :brief brief params return)
     (format nil "~%/**~%*@brief ~a~%*@param ~a~%*@return ~a ~%*/" brief params return))
    ((list :include name)
     (format nil "#include ~S~%" (to-c-name name)))
    ((list :set identifier value )
     (format nil "~a = ~a;~%"
             (match-expr identifier :no-fn-call t)
             (match-expr value)))
    ((list :define term newterm)
     (format nil "#define ~a ~a~%" (to-c-name term) newterm))
    ((list :typedef :struct name newname)
     (format nil "~%typedef struct ~a ~a ~a;~%"
             (to-c-name name)
             (to-c-name name)
             (to-c-name newname)))
    ((list :typedef (list* :struct name a))
     (progn
       (format nil "~%typedef struct ~a ~%{~%~a} ~a;~%"
               (to-c-name name)
               (match-type-name-decls a)
               (to-c-name name))))

    ((list :typedef (list* :enum name a))
     (progn
       (format nil "~%typedef enum ~%{~%~{~a~^,~%~}~%} ~a;~%"
               (loop :for v :in a
                     :when (listp v)
                       :collect (format nil "~a = ~a"
                                        (to-c-name (first v))
                                        (match-expr (second v)))
                     :else
                       :collect (to-c-name v))
               (to-c-name name))))
    ((list :function name args return body)
     (format nil "~%~a~a(~a)~%{~%~a}~%"
             (match-type return)
             (to-c-name name)
             (match-type-name-args args)
             (match-statement body)
             ))
    ((list :function name args return)
     (format nil "~%~a~a(~a);~%"
             (match-type return)
             (to-c-name name)
             (match-type-name-args args)))
    (_ (error "unknown top form: ~a" form))))



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







(defun expand-evals (form)
  (cond
    ((and (listp form)
          (eq (first form) :eval)
          (eq (length form) 2))
     (let ((e (eval (second form))))
       (values (if (listp e)
                   (expand-evals (replace-macros e))
                   (list e))
               t)))
    ((and (listp form)
          (eq (first form) :eval-as-is)
          (eq (length form) 2))
     (values (eval (second form)) nil))
    ((listp form)
     (let ((result ()))
       (loop :for item :in form
             :do (multiple-value-bind (f e) (expand-evals
                                             (replace-macros item))
                   (if e
                       (setf result (concatenate 'list result f))
                       (setf result (append result (list f)))
                       )))
       result))
    (t form))
  )



(defun replace-single-template-var (form value)
  (cond
    ((eq form :template-var)
     value)
    ((listp form)
     (loop :for x :in form
           :collect (replace-single-template-var x value)))
    (t form)))



(defun replace-template-var (form target value)
  (cond
    ((and (listp form)
          (eq (first form) :template)
          (eq (length form) 2)
          (eq (second form) target))
     value)
    ((listp form)
     (loop :for x :in form
           :collect (replace-template-var x target value)))
    (t form)))


(defun genc (form)
  (let ((evald-form (expand-evals form)))
    (match-top-level evald-form)))
