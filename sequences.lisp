(in-package :screamer+)

(defun mapv (result-type function &rest sequences)
 (apply #'funcallv #'map result-type function sequences))

(defun everyv (predicate &rest sequences)
  (applyv #'every (cons predicate sequences)))

(defun somev (predicate &rest sequences)
  (applyv #'some (cons predicate sequences)))

(defun notanyv (predicate &rest sequences)
  (applyv #'notany (cons predicate sequences)))

(defun noteveryv (predicate &rest sequences)
  (applyv #'notevery (cons predicate sequences)))

(defun subseqv (sequence start &optional end)
  (funcallv #'subseq sequence start end))

(defun countv (item list &key (test #'eql))
  "Returns a variable constrained to be the number of times ITEM occurs in LIST, using TEST."
  (funcallv #'count item list :test test))

(defun remove-duplicatesv (list &key (test #'equal))
 (funcallv #'remove-duplicates list :test test))

(defun lengthv (x)
 (funcallv #'length x))

