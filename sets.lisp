(in-package :screamer+)

(defun a-set-ofv (test)
 "Original from Screamer-Plus.
Models a set as a list
Each member of the set must produce t when the test is applied to it.
Test is a deterministic function which returns a boolean value."
  (let ((z (make-variable))) ;(a-listv)))
    (assert! (everyv test z))
    z))

(defun same-elementsv (x)
  "Returns a variable constrained to have the same elements as x (order may differ)."
  (let ((z (make-variable)))
    (assert! (set-equalv z x))
    z))

(defun subsetpv (x y)
"Original from Screamer-Plus
This function returns a boolean variable constrained to indicate
whether x is a subset of y."
  (let ((y y) (x x))
    (everyv #'(lambda(d) (memberv d y)) x)))

(defun set-equalv (x y)
  (andv (subsetpv x y) (subsetpv y x)))

(defun bag-equalv (x y)
  "Returns a variable constrained to indicate whether the bags x and y are equal.
Two bags are equal when they contain the same numbers of each of their elements."
  (let ((z (a-booleanv)))
    (equalv z
            (andv (everyv (lambda (e) (=v (countv e x) (countv e y))) x)
                  (everyv (lambda (e) (=v (countv e x) (countv e y))) y)))
    z))

(defun a-subset-ofv (x)
  (let ((z (make-variable)))
    (assert! (everyv #'(lambda (d) (memberv d x)) z))
    z))

(defun intersectionv (list1 list2 &key key test test-not)
  (apply #'funcallv
         #'intersection
         (append (list list1 list2)
                 (when key      (list :key key))
                 (when test     (list :test test))
                 (when test-not (list :test-not test-not)))))

(defun unionv (list1 list2 &key key test test-not)
  (apply #'funcallv
         #'union
         (append (list list1 list2)
                 (when key      (list :key key))
                 (when test     (list :test test))
                 (when test-not (list :test-not test-not)))))