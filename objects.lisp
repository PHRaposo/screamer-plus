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

(defun make-instancev (class &rest initargs)
  (apply #'funcallv #'make-instance (cons class initargs)))

(defun classpv (obj)
  (funcallv #'class-of obj))

(defun class-namev (obj)
  (funcallv (lambda (x)
   (apply #'class-name (list (class-of x)))) obj))

  (defun class-ofv (obj)
    (funcallv (lambda (x)
                 (apply #'class-of (list (value-of x)))) obj))

(defun slot-exists-pv (obj slotname)
  (funcallv (lambda (x)
               (apply #'slot-exists-p (list (value-of x) (value-of slotname)))) obj))

(defun slot-names-ofv (obj)
  (funcallv #'slot-names-of obj))

(defun slot-boundpv (obj slotname)
  (funcallv #'slot-boundp obj slotname))

(defun slot-valuev (objvar slotname)
 (funcallv #'slot-value objvar slotname))

(defun reconcile-objectsv (objvar1 objvar2)
  (let ((slots1 (slot-names-ofv objvar1))
        (slots2 (slot-names-ofv objvar2)))
    (assert! (equalv slots1 slots2))
    (everyv (lambda (s)
              (equalv (slot-valuev objvar1 s)
                      (slot-valuev objvar2 s)))
            slots1)))

(defun reconcile (var1 var2)
  (cond 
   ((equal var1 var2) t)
   ((and (objectp var1) (objectp var2))
    (reconcile-objectsv var1 var2))
   (t (reconcile-objectsv var1 var2))))
