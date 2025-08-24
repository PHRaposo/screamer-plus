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