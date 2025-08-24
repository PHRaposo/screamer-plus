(in-package :screamer+)

(defun consv (x y)
 (funcallv #'cons x y))

(defun carv (x)
 (funcallv #'car x))

(defun cdrv (x)
 (funcallv #'cdr x))

(defun restv (x)
  (funcallv #'rest x))

(defun nthv (n lst)
  (funcallv #'nth n lst))

(defun firstv (x)
 (funcallv #'first x))

(defun secondv (x)
 (funcallv #'second x))

(defun thirdv (x)
 (funcallv #'third x))

(defun fourthv (x)
 (funcallv #'fourth x))

(defun fifthv (x)
 (funcallv #'fifth x))

(defun sixthv (x)
 (funcallv #'sixth x))

(defun seventhv (x)
 (funcallv #'seventh x))

(defun eighthv (x)
 (funcallv #'eighth x))

(defun ninthv (x)
 (funcallv #'ninth x))

(defun tenthv (x)
 (funcallv #'tenth x))

(defun make-listv (size &key (initial-element '(make-variable)))
 (if (variable? size)
     (error "The current implementation does not allow size to be
           an unbounded variable.")
    (let ((listv '()))
      (dotimes (c (value-of size))
        (setq listv (nconc listv (list (eval initial-element)))))
      listv)))

(defun mapcarv (function &rest lists)
 (apply #'funcallv #'mapcar function lists))

(defun maplistv (function &rest lists)
 (apply #'funcallv #'maplist function lists))

(defun listv (&rest elements)
 (applyv #'list elements))

(defun appendv (&rest lists)
 (applyv #'append lists))

(defun at-mostv (n fn &rest xs)
  (let ((z (a-booleanv)) 
        (count-trues (sumv (mapcarv #'reifyv (apply #'mapv 'list fn xs)))))
    (assert! (impliesv z (<=v count-trues n)))
    (assert! (impliesv (notv z) (>=v count-trues n)))
    (assert!-true z)    
    z))

(defun at-leastv (n fn &rest xs)
  (let ((z (a-booleanv)) 
        (count-trues (sumv (mapcarv #'reifyv (apply #'mapv 'list fn xs)))))
    (assert! (impliesv z (>=v count-trues n)))
    (assert! (impliesv (notv z) (<=v count-trues n)))
    (assert!-true z)
    z))

(defun exactlyv (n fn &rest xs)
  (let ((z (a-booleanv)) 
        (count-trues (sumv (mapcarv #'reifyv (apply #'mapv 'list fn xs)))))
    (assert! (impliesv z (=v count-trues n)))
    (assert! (impliesv (notv z) (/=v count-trues n)))
    (assert!-true z)
    z))
