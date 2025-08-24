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
