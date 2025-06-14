(in-package :cl-cgen)


(defparameter *tags* (make-hash-table :test #'equal))


(defun tag-get (tag)
  (gethash tag *tags*))

(defun tag-remove (setid value)
  (let ((s (gethash setid *tags*)))
    (if (listp s)
        (progn
          (setf (gethash setid *tags*) (remove value s :test #'equal)))
        (error "~a is not a set, it is ~a" setid s))))

(defun tag-assoc (setid value)
  (multiple-value-bind (s exists) (gethash setid *tags*)
    (if exists
        (if (listp s)
            (progn
              (setf (gethash setid *tags*) (adjoin value s :test #'equal)))
            (error "~a is not a set, it is ~a" setid s))
        (progn (setf (gethash setid *tags*) (list value))))))



(assert
 (null (tag-get :test-1 )))


(assert
 (equal
  (progn (tag-assoc :test-2 :v1)
         (tag-get :test-2))
  (list :v1)))


(assert
 (equal
  (progn
    (tag-assoc :test-3 :v1)
    (tag-assoc :test-3 :v2)
    (tag-assoc :test-3 :v1)
    (tag-get :test-3))
  (list :v2 :v1)))

(assert
 (null
  (progn
    (tag-assoc :test-4 :v2)
    (tag-remove :test-4 :v2)
    (tag-get :test-4))))
