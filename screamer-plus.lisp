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

(defvar *screamer+-version* (asdf:component-version (asdf:find-system :screamer-plus))
  "The version of Screamer-Plus which is loaded.")

;;; An extension to the SCREAMER variable definition extends the allowable
;;; nonnumber-types

#-screamer-clos
(defstruct (variable+ 
      (:print-function print-variable+)
      (:include screamer::variable)
      (:constructor make-variable+))
  ;; Stores the type of the variable if known
  nonnumber-type)

#+screamer-clos
(defclass variable+ (screamer::variable)
  ((nonnumber-type :accessor variable+-nonnumber-type :initform nil)))

(defun make-variable (&optional (name nil name?))
 (let ((variable (make-variable+ :name (if name? name (incf screamer::*name*)))))
   (setf (variable+-value variable) variable)   
   variable))

(define-symbol-macro ?? (make-variable))

(defmethod variable-type-known? ((x variable+))
  (not (null (variable+-nonnumber-type x))))

(defmethod variable-get-type ((x variable+))
  (variable+-nonnumber-type x))

(defun print-variable+ (x stream print-level)
  (declare (ignore print-level))
  (let ((x (value-of x)))
    (cond
      ((variable? x) (if (and (not (equal (variable-enumerated-domain x) t))
                           (not (null (variable-enumerated-antidomain x))))
                       (error "This shouldn't happen"))
        (format stream "[~S" (screamer::variable-name x))       
        (format stream "~A"
          (cond ((variable-type-known? x) (format nil " ~(~a~)" (variable-get-type x)))
          ((screamer::variable-boolean? x) " Boolean")
          ((screamer::variable-real? x)
          (cond
            ((screamer::variable-rational? x)
              (cond
                ((screamer::variable-integer? x) " integer")
                ((screamer::variable-noninteger-rational? x) " noninteger-rational")
                (t " rational")))
            (t
              (cond
                ((screamer::variable-noninteger? x)
                 (if (screamer::variable-nonrational? x)
                      " nonrational-real" " noninteger-real"))
                (t " real")))))
                     ((screamer::variable-number? x)
                      (cond ((screamer::variable-nonreal? x) " nonreal-number")
                            ((screamer::variable-noninteger? x) " noninteger-number")
                            ((screamer::variable-nonrational? x) " nonrational-number")
                            ((screamer::variable-rational? x) " rational-number")
                            (t " number")))
                     ((screamer::variable-nonnumber? x) " nonnumber")
                     ((screamer::variable-nonreal? x) " nonreal")
                     ((screamer::variable-noninteger? x) " noninteger")
                     (t "")))       
        (if (screamer::variable-real? x)
          (if (screamer::variable-lower-bound x)
      (if (screamer::variable-upper-bound x)
              (format stream " ~D:~D" (screamer::variable-lower-bound x)
                (screamer::variable-upper-bound x))
              (format stream " ~D:" (screamer::variable-lower-bound x)))
      (if (screamer::variable-upper-bound x)
              (format stream " :~D" (screamer::variable-upper-bound x)))))
        (if (and (screamer::variable-rational? x)
                 (screamer::variable-possibly-noninteger-rational? x)
                 (integerp (screamer::variable-max-denom x)))
         (format stream " max-denom:~D" (screamer::variable-max-denom x)))        
        (if (and (not (equal (screamer::variable-enumerated-domain x) t))
              (not (screamer::variable-boolean? x)))
          (format stream " enumerated-domain:~S" (screamer::variable-enumerated-domain x)))        
        (if (not (null (screamer::variable-enumerated-antidomain x)))
          (format stream " enumerated-antidomain:~S" (screamer::variable-enumerated-antidomain x)))
        (format stream "]"))     
      (t (format stream "~S" x)))))

(defun slot-names-of (obj)
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (class-of obj))))

(defun objectp (var)
  "Determines whether a variable is a standard CLOS object or not"
  (typep var 'standard-object))

(defun eqv (x y)
  "Original from Screamer-Plus.
  A simple version of eqv for atoms. PROPAGATION PROPERTIES: as for funcallv."
  (funcallv #'eq x y))

(defun impliesv (p q)
  "Original from Screamer-Plus.
  This was not included in the standard SCREAMER distribution.
  The following generates the truth table for implication:
  > (setq p (a-booleanv))
  [3558 Boolean]
  > (setq q (a-booleanv))
  [3559 Boolean]
  > (setq r (a-booleanv))
  [3560 Boolean]
  > (assert! (equalv r (impliesv p q)))
  NIL]
  > (all-values (solution (list p '=> q 'is r) (static-ordering #'linear-force)))
  ((T => T IS T) (T => NIL IS NIL) (NIL => T IS T) (NIL => NIL IS T))
  PROPAGATION PROPERTIES: as for other logical functions."
  (orv (notv p) q))

(screamer::defmacro-compile-time carefully (&body forms)
 "Redesigned for original Screamer-Plus.
  Evaluates FORMS, returning its value or NIL on error, emitting a warning."
  `(handler-case
       (progn ,@forms)
     (error (e)
       (warn "~s failed: ~a" ',forms e)
       nil)))

(screamer::defmacro-compile-time carefully-evaluate (form)
  "Evaluates FORM, returning its value or NIL on error, emitting a warning."
  `(handler-case
       ,form
     (error (e)
       (warn "~s failed: ~a" ',form e)
       nil)))

(screamer::defmacro-compile-time ifv (condition then &optional else)
  "Redesigned for original Screamer-Plus."
  (let ((g-cond (gensym "COND"))
        (g-then (gensym "THEN"))
        (g-else (gensym "ELSE")))
    `(funcallv (lambda (,g-cond ,g-then ,g-else)
                 (if (not (null ,g-cond)) ,g-then ,g-else))
               ,condition ,then ,else)))

 (screamer::defmacro-compile-time condv (&rest clauses)
  (let* ((args (mapcar (lambda (_) (gensym "ARG")) clauses))
         (cond-clauses
           (mapcar #'list args
                   (mapcar (lambda (clause)
                             `(progn ,@(cdr clause)))
                           clauses))))
    `(funcallv
      (lambda ,args
        (cond
          ,@cond-clauses))
      ,@(mapcar #'first clauses))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions: listpv, conspv, symbolpv, stringpv
;;; 
;;; Function: a-typed-varv
;;; Function: typepv
;;; Functions: a-listv, a-consv, a-symbolv, a-stringv
;;;;
;;;; IN-PROGRESS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun formatv (destination control-string &rest args)
"Redesigned for original Screamer-Plus."
 (applyv #'format (apply #'list* destination control-string args)))

(defun reifyv (x)
"Redesigned for original Screamer-Plus."
 (cond ((known?-true x) 1)
       ((known?-false x) 0)
       (t (let* ((z (an-integer-betweenv 0 1)))
           (assert!-true (funcallv #'(lambda (v) (if (eq v t) 1 0)) z))
           z))))

(defun sumv (listv)
  (funcallv #'(lambda (lst) (apply #'+ lst)) listv))

;; compatibility note
;; This section contains or original functions from original Screamer-Plus or
;; adaptations of them, keeped here for backward compatibility.
;; Some of them are deprecated.

(defmacro-compile-time make-equal (var value &optional (retval '(fail)))
"Original for original Screamer-Plus."
  `(if (possibly? (equalv ,var ,value))
       (progn
   (assert! (equalv ,var ,value))
   (values ,var))
     (progn
       (warn "(make-equal ~s ~s) failed~%  ~s = ~s; ~s = ~s"
       (quote ,var) (quote ,value)
       (quote ,var) ,var
       (quote ,value) ,value)
       (values ,retval))))

(defun all-different2 (x xs)
"Original for original Screamer-Plus."
  (if (null xs)
      t
      (andv (notv (funcallv #'equal x (car xs)))
            (all-different2 x (cdr xs))
            (all-different2 (car xs) (cdr xs)))))

(defun all-differentv (x &rest xs)
"Original for original Screamer-Plus."
  (all-different2 x xs))

(defun constraint-fn (f)
"Redesigned for original Screamer-Plus."
  (alexandria:curry (lambda (&rest args)
                      (value-of (applyv (value-of f) args)))))

(defun members-ofv (x)
"DEPRECATED. Use remove-duplicatesv instead."
 (remove-duplicatesv x))

(defun not-equalv (x y &key (full-propagation nil))
"Redesigned for original Screamer-Plus.
DEPRECATED. Use (notv (equalv ...)) instead."
 (declare (ignore full-propagation))
 (let ((z (a-booleanv)))  
  (assert! (equalv z (funcallv #'(lambda (a b) (not (equal a b))) x y)))
  z))

(defun funcallgv (f &rest x)
"DEPRECATED. Use funcallv instead."
 (funcallv f x))

(defun funcallinv (f inverse &rest el)
"DEPRECATED. Use funcallv instead."
 (declare (ignore inverse))
  (let* ((z (applyv f el)))
    z))

(defmacro-compile-time setq-domains (vars vals &aux (res nil))
"Original for original Screamer-Plus."
 (dolist (var vars) (setq res (nconc (list var vals) res)))
 (cons 'setq res))

;; note: The following functions are nondeterministic generators
;; for subsets and partitions not included in the classic Screamer.
;; They were described in:
;; SCREAMER: A Portable Efficient Implementation of Nondeterministic
;; Common Lisp, by Jeffrey Mark Siskind and David Allen McAllester.

(defun a-subset-of (x)
 "Nondeterministically generates all possible subsets of X."
  (if (null (value-of x))
       nil
      (let ((y (a-subset-of (cdr x))))
        (either (cons (car x) y) y))))

(defun a-partition-of (x)
 "Nondeterministically generates all possible partitions of X."
  (if (null x)
       nil
      (let ((y (a-partition-of (cdr x))))
        (either (cons (list (car x)) y)
                (let ((z (a-member-of y)))
                  (cons (cons (car x) z)
                        (remove z y :test #'equal :count 1)))))))
