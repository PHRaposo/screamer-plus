 ;; TESTS FROM SCREAMER-PLUS DOCUMENTATION
 ;; by Simon White

(in-package :screamer-plus)

(defun equal-set? (x y)
 (and (subsetp x y :test #'equal) (subsetp y x :test #'equal)))

 (defun test-listpv ()
 (let ((a (a-member-ofv '(1 1/2 2.5 #C(2 3) nil t (HELLO) "HELLO" HELLO))))
  (assert! (listpv a)) 
  (known? (memberv a '(nil (HELLO))))))

 (defun test-stringpv ()
 (let ((a (a-member-ofv '(1 1/2 2.5 #C(2 3) nil t (HELLO) "HELLO" HELLO))))
  (assert! (stringpv a))
  (known? (equalv a "HELLO"))))

(defun test-symbolpv ()
 (let ((a (a-member-ofv '(1 1/2 2.5 #C(2 3) nil t (HELLO) "HELLO" HELLO))))
  (assert! (symbolpv a))
  (known? (memberv a '(nil t HELLO)))))

 (defun test-listv ()
 (let ((a (a-listv)))
  (assert! (memberv a '(1 1/2 2.5 #C(2 3) nil t two "THREE" (four)))) 
  (known? (memberv a '(nil (four))))))

 (defun test-stringv ()
 (let ((a (a-stringv)))
  (assert! (memberv a '(1 1/2 2.5 #C(2 3) nil t two "THREE" (four))))
  (known? (equalv "THREE" a))))

 (defun test-symbolv ()
 (let ((a (a-symbolv)))
  (assert! (memberv a '(1 1/2 2.5 #C(2 3) nil t two "THREE" (four))))
  (known? (memberv a '(nil t two)))))

(defun test-booleanv-symbolv ()
 (let ((x (a-member-ofv '(t nil foo bar 1 2 3))))
  (assert! (symbolpv x))
  (andv (known? (memberv x '(t nil foo bar)))
        (progn (assert! (booleanpv x))
               (known? (notv (memberv x '(foo bar))))))))

;; note: the functions TYPEPV and A-TYPED-VARV were removed from this version.
;; The new should be defined using screamer-define-type macro and screamer-define-generator-function.

;; note: The current IFV version does not handle recursive constraint definitions,
;;       like the original version from Screamer-Plus did.

(defun test-ifv-1 ()
 (let* ((x (a-member-ofv '(1 two "THREE" (four))))
       (result (make-variable))) (assert! (equalv result (ifv (integerpv x)
                     'INTEGER
                     'NON-INTEGER)))
  (known? (memberv result '(INTEGER NON-INTEGER)))))

(defun test-ifv-2 ()
 (let* ((x (a-member-ofv '(1 two "THREE" (four))))
       (result (ifv (integerpv x) t nil)))
   (assert! result)
   (known? (=v x 1))))

(defun test-impliesv-1 ()
  (let* ((x (a-booleanv))
         (y (a-booleanv)))
    (assert! (impliesv x y))
    (make-equal x t)
    (known? (equalv t y))))

(defun test-impliesv-2 ()
  (let* ((p (a-booleanv))
         (q (a-booleanv))
         (r (a-booleanv)))
    (assert! (equalv r (impliesv p q)))
    ;; This should return a list of all combinations of p, q, and r
    (equal '((T => T IS T) (T => NIL IS NIL) (NIL => T IS T) (NIL => NIL IS T))
     (all-values (solution (list p '=> q 'is r) (static-ordering #'linear-force))))))

;; note: the MY-MEMBERV function from original Screamer-Plus doesn't work
;; in this version. 

(defun test-make-equal ()
  (let ((x (make-variable)))
    (make-equal x '(1 2 3))
    (equal '(1 2 3) (value-of x))))

(defun test-condv ()
  (let ((x (a-member-ofv '(1 2 3)))
        (result (make-variable)))
    (assert! (equalv result
                     (condv
                       ((=v x 1) :a)
                       ((=v x 2) :b)
                       ((=v x 3) :c))))
    (equal-set? '((1 :a) (2 :b) (3 :c))
    (all-values (solution (list x result) (static-ordering #'linear-force))))))

(defun test-firstv ()
 (let* ((x (make-variable))
        (z (firstv x)))
     (make-equal x '(a b c))
     (known? (equalv 'a z))))

(defun test-nthv-1 ()
  (let* ((x (make-variable))
         (z (nthv 2 x)))
      (make-equal x '(a b c d e f))
      (known? (equalv 'c z))))
    
(defun test-nthv-2 ()
  (let ((a (make-listv 4)))
    (assert! (equalv (nthv 1 a) 'foo))
    (known? (equalv 'foo (secondv a)))))

(defun test-subseqv-1 ()
 (let* ((n (make-variable))
        (a (subseqv '(1 2 3 4 5 6) n)))
    (make-equal n 3)
    (known? (equalv '(4 5 6) a))))

(defun test-subseqv-2 ()
 (let ((x (make-listv 6))
       (z (make-listv 2)))
(assert! (integerpv (firstv z)))
(assert! (integerpv (secondv z)))
(assert! (equalv z (subseqv x 2 4)))
(known? (andv (integerpv (thirdv x))
              (integerpv (fourthv x))))))

(defun test-lengthv ()
 (let* ((x (make-variable))
        (len (lengthv x)))
    (assert! (memberv x '("one" "two" "three" "four")))
(andv (known? (memberv len '(3 4 5)))
      (progn (make-equal x "three")
             (known? (=v 5 len))))))

(defun test-consv-1 ()
  (let* ((x (make-variable))
        (z (consv 'g x)))
    (make-equal x '(1 2 3))
    (known? (equalv '(g 1 2 3) z))))

(defun test-consv-2 ()
  (let* ((x (make-variable))
        (y (make-variable))
        (z (consv x y)))
    (make-equal z '(a b c))
    (andv (known? (equalv x 'a))
          (known? (equalv y '(b c))))))

(defun test-carv-1 ()
  (let* ((x (make-variable))
        (z (carv x)))
    (make-equal x '(fee fi fo fum))
    (known? (equalv 'fee z))))

(defun test-carv-2 ()
  (let* ((x (make-variable))
        (z (carv x)))
    (make-equal x (cons 'one 'two))
    (known? (equalv 'one z))))

(defun test-cdrv-1 ()
  (let* ((x (make-variable))
        (z (cdrv x)))
    (make-equal x '(1 2 3 4))
    (known? (equalv '(2 3 4) z))))

(defun test-cdrv-2 ()
;; - This variant does not propagate constraints through variables that lack an
;;  explicitly enumerated finite domain. In other words, only variables whose
;;  possible values are enumerated (finite and known) participate in forward
;;  domain narrowing.
;; - x is constrained to be one of the three concrete lists: (a b c), (d e f), or (g h i).
;; - carv applied to x yields a “head” logic variable whose domain is the set of
;;   the first elements across the candidates: {a, d, g}.
;; - cdrv applied to x yields a “tail” logic variable whose domain is the set of
;;   the remaining pairs: {(b c), (e f), (h i)}.
;; - Imposing head = g and tail = (h i) prunes x’s domain to the single list (g h i).
;; - known? (equalv '(g h i) x) then succeeds because, under this variant’s
;;   propagation rules, x becomes fully determined (entailed) by the given constraints.
(let* ((x (a-member-ofv '((a b c) (d e f) (g h i))))
        (head (carv x)) ; domain (a d g)
        (tail (cdrv x))) ; domain ((b c) (e f) (h i))
    (make-equal head 'g)
    (make-equal tail '(h i))
    (known? (equalv '(g h i) x))))

(defun test-appendv-1 ()
(let* ((x (make-variable))
       (z (appendv x '(3 4))))
    (make-equal x '(1 2))
    (known? (equalv '(1 2 3 4) z))))

(defun test-appendv-2 ()
 (let* ((*strategy* :ac)
         (x (a-member-ofv '(() (a) (b) (c) (a b) (a c) (b c) (a b c))))
         (y (a-member-ofv '(() (a) (b) (c) (a b) (a c) (b c) (a b c))))
         (z (appendv x y)))
    (assert! (equalv z '(a b c)))
    (andv (known? (memberv x '(() (a) (a b) (a b c))))
          (known? (notv (memberv x '((a c) (b) (c) (b c)))))
          (known? (memberv y '((a b c) (b c) (c) ())))
          (known? (notv (memberv y '((a b) (a c) (a) (b))))))))

;; TODO: MAKE-LISTV, ALL-DIFFERENTV (REMOVED), SET-EQUALV, INTERSECTIONV, UNIONV, BAG-EQUALV, 
;; SUBSETPV, MAKE-ARRAYV, AREFV, MAKE-INSTANCEV, SLOT-VALUEV, CLASSPV, CLASS-OFV, CLASS-NAMEV,
;; SLOT-EXISTS-PV, RECONCILE, MAPCARV, MAPLISTV, AT-LEASTV, AT-MOSTV, EXACTLYV, CONSTRAINT-FN (DEPRECATED)
;; FUNCALLINV (DEPRECATED), FORMATV.

;; EXAMPLES: MASTERMIND and CAR-SEQUENCING-PROBLEM (in screams+.lisp)

(defun prime-ordeal-plus ()
  (let ((bug? nil))
    (flet ((run-test (fn)
            (let ((result
                    (handler-case
                        (funcall fn)
                      (error (e)
                        (format t "Error in ~A: ~A~%" fn e)
                        nil))))
              (unless result
                (format t "~%Test failed: ~A~%" fn)
                (setf bug? t)))))
      (mapc #'run-test
            '(test-listpv
              test-stringpv
              test-symbolpv
              test-ifv-1
              test-ifv-2
              test-listv
              test-stringv
              test-symbolv
              test-impliesv-1
              test-impliesv-2
              test-make-equal
              test-condv
              test-firstv
              test-nthv-1
              test-nthv-2
              test-subseqv-1
              test-subseqv-2
              test-lengthv
              test-consv-1
              test-consv-2
              test-carv-1
              test-carv-2
              test-cdrv-1
              test-cdrv-2
              test-appendv-1
              test-appendv-2)))
    (when bug?
      (error "Screamer Plus has a bug"))
    t))
