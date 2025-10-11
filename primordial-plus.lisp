 ;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;
;;;; Screamer-Plus: A modernized constraint logic programming library for Common Lisp
;;;;
;;;; Screamer-Plus is an extension of constraint propagation in Screamer,
;;;; built upon a fundamental redesign of the core functions `funcallv` and `applyv`
;;;; introduced in version 4.0.1 of Screamer.
;;;;
;;;; This new foundation enables automatic constraint propagation, eliminating the
;;;; need for manual noticers and simplifying function/macro definitions.
;;;; As a result, many of the macros and functions originally found in Screamer-Plus
;;;; (by Simon White) — such as `CARV`, `CDRV`, `IFV`, and others — have been
;;;; entirely rewritten or reimagined with cleaner semantics and greater efficiency.
;;;;
;;;; Some function names and general ideas are inspired by the original Screamer-Plus
;;;; by Simon White, but all code in this package is original unless otherwise noted.”
;;;;
;;;; Contributions, feedback, and extensions are welcome.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Copyright (c) 2025 Paulo Henrique Raposo
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;;; this software and associated documentation files (the "Software"), to deal in
;;;; the Software without restriction, including without limitation the rights to
;;;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;;;; the Software, and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; The above copyright and authorship notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;;;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;;;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;;;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; THIS FILE CONTAINS TESTS FROM SCREAMER-PLUS DOCUMENTATION
;;;; by Simon White
;;;; ALONG WITH NEW TESTS FOR THE NEW FUNCTIONS AND FUNCTIONALITY

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

;; note: This version of IFV does not allow recursive calls. 

(defun test-ifv-1 ()
 (let* ((x (a-member-ofv '(1 two "THREE" (four))))
       (result (make-variable)))
  (assert! (equalv result
                  (ifv (integerpv x)
                        'INTEGER
                        'NON-INTEGER)))
  (equal-set? '(INTEGER NON-INTEGER)
    (all-values (solution result (static-ordering #'linear-force))))))

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

;; note: this version of MY-MEMBERV uses IFV-REC,
;; which allows recursive calls (unlike the new IFV).

(defun my-memberv (m ll)
 (when ll
     (ifv-rec (equalv m (carv ll))
      t
     (my-memberv m (cdrv ll)))))

(defun test-my-memberv-ifv-rec ()
 (let* ((x (make-variable))
       (z (my-memberv 1 x)))
   (make-equal x '(3 2 1))
   (known?-true z)))

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
 (equal-set? '(3 3 5 4)
  (all-values (solution len (static-ordering #'linear-force))))))

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
    (known? (andv (equalv x 'a)
                  (equalv y '(b c))))))

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

(defun test-nthcdrv ()
(let* ((x (a-member-ofv '((0 1 2) (3 4 5) (6 7 8))))
       (z (nthcdrv 1 x)))
 (known? (memberv z '((1 2) (4 5) (7 8))))))

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
    (known? (andv (memberv x '(() (a) (a b) (a b c)))
                  (notv (memberv x '((a c) (b) (c) (b c))))
                  (memberv y '((a b c) (b c) (c) ()))
                  (notv (memberv y '((a b) (a c) (a) (b))))))))

(defun test-make-listv ()
 (let* ((n (make-variable))
        (z (make-listv n)))
    (make-equal n 4)
    (assert! (equalv (nthv 1 z) 'foo))
    (known? (andv (=v (lengthv z) 4)
                  (equalv (secondv z) 'foo)))))

(defun test-all-differentv ()
 ;; note: this is currently a lifted function from Screamer 4.0.1. 
 (let ((variables (n-variables 3 'a-member-ofv '(0 1 2))))
  (assert! (all-differentv variables))
  (equal-set? '((0 1 2) (0 2 1) (1 0 2) (1 2 0) (2 0 1) (2 1 0))
   (all-values (solution variables (static-ordering #'linear-force))))))

(defun test-set-equalv ()
 (let* ((x '(a b c))
        (y (n-variables 4 'a-member-ofv '(a b c)))
        (same-set (set-equalv x y)))
  (assert! same-set)
  (every #'identity
   (all-values (third (solution (list x y same-set) (static-ordering #'linear-force)))))))

(defun test-intersectionv-1 ()
  (let* ((x (make-variable))
         (i (intersectionv x '(a a b c c c))))
    (make-equal x '(a b d e f))
    (equal-set? '(a b) (value-of i))))

(defun test-unionv-1 ()
  (let* ((x (make-variable))
        (u (unionv x '(a a b c c c))))
    (make-equal x '(a b d e f))
   (equal-set? '(a b c d e f) (value-of u))))

(defun test-bag-equalv ()
(let* ((x (n-variables 6 'a-member-ofv '(a b c))) 
       (y '(c a c b c a))
       (same-bag (bag-equalv x y)))
 (assert! same-bag)
 (every #'identity 
  (all-values (third (solution (list x y same-bag) (static-ordering #'linear-force)))))))

(defun test-subsetv ()
 (let* ((*strategy* :ac)
        (x (n-variables 6 'a-member-ofv '(a b c d e f)))
        (y '(a b c))
        (sub (subsetpv x y)))
  (assert! sub)
  (known? (andv (memberv (firstv x) '(a b c))
                (memberv (secondv x) '(a b c))
                (memberv (thirdv x) '(a b c))
                (memberv (fourthv x) '(a b c))
                (memberv (fifthv x) '(a b c))
                (memberv (sixthv x) '(a b c))))))

(defun test-a-subset-ofv ()
 (let* ((x (a-subset-ofv '(0 1 2 3))))
        (known? (memberv x
                        '(NIL (0) (1) (0 1) (2) (0 2) (1 2)
                         (0 1 2) (3) (0 3) (1 3) (0 1 3) (2 3)
                         (0 2 3) (1 2 3) (0 1 2 3))))))

(defun array-set-equal? (xs ys)
  (and (every (lambda (x)
                (some (lambda (y) (equalp x y)) ys))
              xs)
       (every (lambda (y)
                (some (lambda (x) (equalp y x)) xs))
              ys)))

(defun test-make-arrayv ()
(let* ((d (make-variable))
       (a (make-arrayv d :initial-element (a-member-ofv '(0 1 2)))))
    (make-equal d '(2 3))
(array-set-equal? '(#2A((2 2 2) (2 2 2))
                    #2A((1 1 1) (1 1 1))
                    #2A((0 0 0) (0 0 0)))
                   (all-values (solution a (static-ordering #'linear-force))))))

(defun test-arefv ()
 (let* ((a (make-array '(2 3) :initial-contents '((p q r) (s t u))))
        (x (make-variable))
        (y (make-variable))
        (val (arefv a x y)))
  (assert! (=v x 0))
  (assert! (=v y 1))
  (known? (equalv 'Q val))))

(defclass junk ()
 ((top :initarg :top)
  (bottom :initarg :bottom)))

(defun test-make-instancev ()
(let* ((x (make-variable))
       (y (make-variable))
       (z (make-variable))
       (instancev (make-instancev z :top x :bottom y)))
 (assert! (equalv z 'junk))
 (assert! (equalv x 'SLOT1))
 (assert! (equalv y 'SLOT2)) 
 (known? (andv (classpv instancev 'junk)
               (equalv (slot-valuev instancev 'top) x)
               (equalv (slot-valuev instancev 'bottom) y)))))

 (defclass person ()
 ((name :accessor name :initarg :name)))

(defun test-slot-valuev ()
 (let* ((anon (make-instance 'person :name (make-variable)))
        (name-of-anon (slot-valuev anon 'name)))
  (make-equal name-of-anon "Fred Bloggs")
  (known? (equalv (slot-valuev anon 'name) "Fred Bloggs"))))

(defun test-classpv.1 ()
 (let* ((x (make-variable))
        (z (classpv x 'junk)))
  (make-equal x (make-instance 'junk))
  (known?-true z)))

(defun test-classpv.2 ()
 (let ((x (a-member-ofv (list 1 'hello (make-instance 'junk) nil "hello"))))
  (assert! (notv (classpv x 'junk)))
  (equal-set? '("hello" NIL HELLO 1)
   (all-values (solution x (static-ordering #'linear-force))))))

(defun test-class-ofv ()
 (let* ((x (make-variable))
        (z (class-ofv x)))
   (make-equal x (make-instance 'junk))
   (known? (equalv (class-namev z) 'JUNK))))

(defclass foo () ())

(defclass bar () ())

(defun test-class-namev ()
 (let* ((x (make-variable))
        (name (class-namev (class-ofv x))))
(assert! (memberv x (list (make-instance 'foo)
                          (make-instance 'bar))))
(known? (memberv name '(FOO BAR)))))

(defclass house () (number))

(defun test-slot-exists-pv ()
(let* ((x (a-member-ofv (list (make-instance 'house)
                              (make-instance 'person)))))
 (assert! (slot-exists-pv x 'name))
 (known? (equalv (class-namev (class-ofv x)) 'person))))

;; TODO: test-reconcile

(defun test-listv-equalv ()
 (let* ((*strategy* :ac)
        (x (screamer::variablize (n-variables 3 'an-integer-betweenv 0 10)))
        (y (a-member-ofv '((0 1 2) (3 4 5) (6 7 8 9) (10 11 12 13)))))
 (assert! (listv-equalv x y))
 (known? (andv (memberv (carv x) '(0 3))
               (memberv (secondv x) '(1 4))
               (memberv (thirdv x) '(2 5))
               (memberv y '((0 1 2) (3 4 5)))))))

(defun test-mapcarv ()
(let* ((a (a-listv))
       (b (a-listv))
       (sums (mapcarv #'+v a b)))
 (make-equal a '(1 2 3))
 (make-equal b '(10 20 30))
 (known? (equalv '(11 22 33) sums))))

(defun test-maplistv ()
 (let* ((a (make-variable))
        (b (maplistv (lambda (x) (cons 'head x)) a)))
  (make-equal a '(1 2 3 4))
  (known? (equalv b '((HEAD 1 2 3 4) (HEAD 2 3 4) (HEAD 3 4) (HEAD 4))))))

(defun test-mapv.1 ()
;; note: since X is a VECTOR containing two VARIABLES, there is no need
;; to use MAPV. CL MAP can be used instead.
 (let ((x (vector (an-integer-betweenv 0 5) (an-integer-betweenv 0 5))))
  (assert! (apply #'andv (map 'list (lambda (el) (<=v el 3)) x)))
  (known? (andv (<=v (arefv x 0) 3)
                (<=v (arefv x 1) 3)))))

(defun test-mapv.2 ()
 (let* ((x (an-integer-betweenv 0 5))
        (y (an-integer-betweenv 0 5))
        (z (funcallv #'vector x y))
        (map (mapv 'vector #'(lambda (el) (<= el 1)) z)))
  (assert! (everyv #'identity map))
  (array-set-equal? '(#(0 0) #(1 0) #(0 1) #(1 1))
   (all-values (solution z (static-ordering #'linear-force))))))

(defun test-everyv.1 ()
;; note: since X is a LIST, we can use
;; (ASSERT! (APPLY #'ANDV (MAPCAR #'INTEGERPV X))).
 (let ((x (make-listv 4)))
   (assert! (apply #'andv (mapcar #'integerpv x)))
    (known? (andv (integerpv (first x))
                  (integerpv (second x))
                  (integerpv (third x))
                  (integerpv (fourth x))))))

(defun test-everyv.2 ()
 (let ((x (a-member-ofv '((0 1) (0 2) (0 3) (1 2) (1 3) (2 3) (3 3)))))
   (assert! (everyv (lambda (el) (>= el 3)) x))
   (known? (equalv x '(3 3)))))

(defun test-somev ()
 (let ((x (a-member-ofv '("abc" "def" "ghi"))))
   (assert! (somev (lambda (el) (equalv el #\h)) x))
   (known? (equalv x "ghi"))))

(defun test-noteveryv ()
(let ((x (n-variables 2 'a-member-ofv '(a b))))
  (assert! (noteveryv (lambda (el) (equalv el 'a)) x))
  (assert! (noteveryv (lambda (el) (equalv el 'b)) x))
  (equal-set? '((A B) (B A))
   (all-values (solution x (static-ordering #'linear-force))))))

(defun test-notanyv ()
(let ((x (n-variables 2 'a-member-ofv '(a b c))))
  (assert! (notanyv (lambda (el) (equalv el 'c)) x))
  (equal-set? '((A A) (A B) (B A) (B B))
   (all-values (solution x (static-ordering #'linear-force))))))

(defun test-at-leastv ()
  (let ((x (n-variables 3 'a-member-ofv '(a b))))
    (assert! (at-leastv 2 (lambda (el) (equalv el 'a)) x))
    (equal-set? '((B A A) (A B A) (A A B) (A A A))
      (all-values (solution x (static-ordering #'linear-force))))))

(defun test-at-mostv ()
  (let ((x (n-variables 3 'a-member-ofv '(a b))))
    (assert! (at-mostv 1 (lambda (el) (equalv el 'a)) x))
    (equal-set? '((B B B) (B B A) (B A B) (A B B))
      (all-values (solution x (static-ordering #'linear-force))))))

(defun test-exactlyv ()
  (let ((x (n-variables 3 'a-member-ofv '(a b))))
    (assert! (exactlyv 2 (lambda (el) (equalv el 'a)) x))
    (equal-set? '((A A B) (A B A) (B A A))
      (all-values (solution x (static-ordering #'linear-force))))))

(defun test-countv ()
  (let* ((lst (list 'a 'b 'c 'a 'b 'a))
         (x (make-variable))
         (y (countv x lst :test #'equal)))
    (make-equal x 'a)
    (known? (=v 3 y))))

(defun test-remove-duplicatesv ()
(let* ((variables (n-variables 10 'an-integer-betweenv 0 10))
       (no-duplicates (remove-duplicatesv variables :test #'eql)))
 (assert! (equalv no-duplicates '(1)))
 (known? (equalv variables '(1 1 1 1 1 1 1 1 1 1)))))

(defun test-funcallinv ()
 (let* ((a (make-variable))
        (b (funcallinv #'reverse #'reverse a)))
  (make-equal b '(one two three))
  (known? (equalv '(three two one) a))))

(defun test-formatv ()
 (let* ((x (make-variable))
        (y (make-variable))
        (z (formatv nil "*** ~a ~a ***" x y)))
   (make-equal x 'hello)
   (make-equal y 'world)
   (known? (equalv z "*** HELLO WORLD ***"))))

(cl::defun prime-ordeal-plus ()
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
              test-listv
              test-stringv
              test-symbolv
              test-booleanv-symbolv
              test-ifv-1
              test-ifv-2
              test-impliesv-1
              test-impliesv-2
              test-my-memberv-ifv-rec
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
              test-nthcdrv
              test-appendv-1
              test-appendv-2
              test-make-listv
              test-all-differentv
              test-set-equalv
              test-intersectionv-1
              test-unionv-1
              test-bag-equalv
              test-subsetv
              test-a-subset-ofv
              test-make-arrayv
              test-arefv
              test-make-instancev
              test-slot-valuev
              test-classpv.1
              test-classpv.2
              test-class-ofv
              test-class-namev
              test-slot-exists-pv
              test-listv-equalv
              test-mapcarv
              test-maplistv
              test-mapv.1
              test-mapv.2
              test-everyv.1
              test-everyv.2
              test-somev
              test-noteveryv
              test-notanyv
              test-at-leastv
              test-at-mostv
              test-exactlyv
              test-countv
              test-remove-duplicatesv
              test-funcallinv
              test-formatv)
            ))
    (when bug?
      (error "Screamer Plus has a bug"))
    t))
