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

;; IN PROGRESS

(in-package :screamer)

(defun variable-type? (x type)
;; X must be a variable.
(and (variable? x)
    (equal (screamer+::variable+-nonnumber-type x) type)))

(defun restrict-type! (x type)
  (unless (possibly-nonboolean-nonnumber? x)
    (fail))
  (when (and (variable-type? x)
              (not (equal (variable-type? x) type)))
    (fail))
  (when (or (eq (variable-value x) x) (not (variable? (variable-value x))))
    (let ((run? nil))
      (when (variable-possibly-integer? x)
        (local (setf (variable-possibly-integer? x) nil))
        (setf run? t))
      (when (variable-possibly-noninteger-rational? x)
        (local (setf (variable-possibly-noninteger-rational? x) nil))
        (setf run? t))
      (when (variable-possibly-noninteger-real? x)
        (local (setf (variable-possibly-noninteger-real? x) nil))
        (setf run? t))
      (when (variable-possibly-nonreal-number? x)
        (local (setf (variable-possibly-nonreal-number? x) nil))
        (setf run? t))
      (when (variable-possibly-boolean? x)
        (local (setf (variable-possibly-boolean? x) nil))
        (setf run? t))
      (when (null (variable-type? x))
        (local (setf (screamer+::variable+-nonnumber-type x) type))
        (setf run? t))
      (when run?
        (when (and (not (eq (variable-enumerated-domain x) t))
                    (some (lambda (v) (not (typep v type))) (variable-enumerated-domain x)))
          (set-enumerated-domain!
            x (remove-if-not (lambda (v) (typep v type)) (variable-enumerated-domain x))))
    (run-noticers x)))))

(defun restrict-nontype! (x type)
  (unless (possibly-nonboolean-nonnumber? x)
    (fail))
  (when (and (variable-type? x) (equal (variable-type? x) type))
    (fail))
  (when (or (eq (variable-value x) x) (not (variable? (variable-value x))))
   (if (and (not (eq (variable-enumerated-domain x) t))
                 (some (lambda (v) (typep v type)) (variable-enumerated-domain x)))
    (set-enumerated-domain!
      x (remove-if (lambda (v) (typep v type)) (variable-enumerated-domain x))))
   (run-noticers x)))


;; MODELS
#|
(defun known?-typepv (x type)
  (let ((x (value-of x)))
    (typecase x
      (variable (variable-type? x type))
      (otherwise (typep x type)))))

(defun known?-notv-typepv (x type)
  (let ((x (value-of x)))
    (typecase x
      (variable (not (variable-type? x type)))
      (otherwise (typep x type)))))

(defun assert!-typepv (x type)
  (let ((x (value-of x)))
    (typecase x
      (variable (restrict-type! x type))
      (otherwise (if (typep x type)
                      nil
                    (fail))))))

(defun assert!-notv-typepv (x)
  (let ((x (value-of x)))
    (typecase x
      (variable (restrict-nontype! x type))
      (otherwise (if (typep x type)
                      (fail))))))

|#

(in-package :screamer+)

(defun typepv (x type)
 (let ((x (value-of x)))
   (typecase x
    (variable? (equal (variable-type? x) type))
    (otherwise (typep x type)))))

(defun conspv (x)
 (typepv x 'cons))

(defun symbolpv (x)
 (typepv x 'symbol))

(defun stringpv (x)
 (typepv x 'string))

(defun typepv (x)
 (typepv x 'type))

(defun a-listv (&optional name)
(let ((x (if name (make-variable name) (make-variable))))
 (screamer::restrict-type! x 'list))
  x)

(defun a-consv (&optional name)
(let ((x (if name (make-variable name) (make-variable))))
 (screamer::restrict-type! x 'cons))
  x)

(defun a-symbolv (&optional name)
 (let ((x (if name (make-variable name) (make-variable))))
   (screamer::restrict-type! x 'symbol))
  x)

(defun a-stringv (&optional name)
 (let ((x (if name (make-variable name) (make-variable))))
   (screamer::restrict-type! x 'string))
  x)

(defun a-typed-varv (type &optional name)
 (let ((x (if name (make-variable name) (make-variable))))
   (screamer::restrict-type! x 'type))
  x)