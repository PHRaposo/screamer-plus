(in-package :screamer+)

(defun arefv (array &rest indices)
  (funcallv #'aref array (car indices)))

(defun make-arrayv (dimensions &key (element-type t) (initial-element nil) (initial-contents nil))
  (apply #'funcallv
         #'make-array
         (append (list dimensions)
                 (when element-type      (list :element-type element-type))
                 (when initial-element   (list :initial-element initial-element))
                 (when initial-contents  (list :initial-contents initial-contents)))))
