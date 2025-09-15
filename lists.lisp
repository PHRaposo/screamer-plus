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

(defun carv (x)
 (let ((x (value-of x)))
 (typecase x
  (list (car x))
  (screamer::variable
   (funcallv #'car x))
  (otherwise (error "Cannot take CARV of ~A~%" x)))))

(defun cdrv (x)
 (let ((x (value-of x)))
 (typecase x 
  (list (cdr x))
  (screamer::variable
   (funcallv #'cdr x))
  (otherwise (error "Cannot take CDRV of ~A~%" x)))))

(defun restv (x)
 (let ((x (value-of x)))
 (typecase x 
  (list (rest x))
  (screamer::variable
   (funcallv #'rest x))
  (otherwise (error "Cannot take RESTV of ~A~%" x)))))

 (defun consv (x y)
 (let* ((z (funcallv #'cons (value-of x) (value-of y)))
        (carv (carv z))
        (cdrv (cdrv z)))
  (assert! (equalv carv x))
  (assert! (equalv cdrv y))
   z))

(defun nthv (n lst)
(let ((n (value-of n))
      (lst (value-of lst)))
 (typecase lst
  (list (typecase n 
         (integer (nth n lst))
         (screamer::variable (funcallv #'nth n lst))
         (otherwise (error "Cannot take NTHV ~A of ~A.~%" n lst))))
  (screamer::variable (funcallv #'nth n lst))
  (otherwise (error "Cannot take NTHV ~A of ~A.~%" n lst)))))

(defun firstv (x)
  (let ((x (value-of x)))
  (typecase x 
    (list (first x))
    (screamer::variable (funcallv #'first x))
    (otherwise (error "Cannot take FIRSTV of ~A.~%" x)))))

(defun secondv (x)
  (let ((x (value-of x)))
  (typecase x 
    (list (second x))
    (screamer::variable (funcallv #'second x))
    (otherwise (error "Cannot take SECONDV of ~A.~%" x)))))

(defun thirdv (x)
  (let ((x (value-of x)))
  (typecase x 
    (list (third x))
    (screamer::variable (funcallv #'third x))
    (otherwise (error "Cannot take THIRDV of ~A.~%" x)))))

(defun fourthv (x)
  (let ((x (value-of x)))
  (typecase x 
    (list (fourth x))
    (screamer::variable (funcallv #'fourth x))
    (otherwise (error "Cannot take FOURTHV of ~A.~%" x)))))

(defun fifthv (x)
  (let ((x (value-of x)))
  (typecase x 
    (list (fifth x))
    (screamer::variable (funcallv #'fifth x))
    (otherwise (error "Cannot take FIFTHV of ~A.~%" x)))))

(defun sixthv (x)
  (let ((x (value-of x)))
  (typecase x 
    (list (sixth x))
    (screamer::variable (funcallv #'sixth x))
    (otherwise (error "Cannot take SIXTHV of ~A.~%" x)))))

(defun seventhv (x)
  (let ((x (value-of x)))
  (typecase x 
    (list (seventh x))
    (screamer::variable (funcallv #'seventh x))
    (otherwise (error "Cannot take SEVENTHV of ~A.~%" x)))))

(defun eighthv (x)
  (let ((x (value-of x)))
  (typecase x 
    (list (eighth x))
    (screamer::variable (funcallv #'eighth x))
    (otherwise (error "Cannot take EIGHTHV of ~A.~%" x)))))

(defun ninthv (x)
  (let ((x (value-of x)))
  (typecase x 
    (list (ninth x))
    (screamer::variable (funcallv #'ninth x))
    (otherwise (error "Cannot take NINTHV of ~A.~%" x)))))

(defun tenthv (x)
  (let ((x (value-of x))) 
  (typecase x 
    (list (tenth x))
    (screamer::variable (funcallv #'tenth x))
    (otherwise (error "Cannot take TENTHV of ~A.~%" x)))))

(defun lengthv (x)
  (let ((x (value-of x)))
  (typecase x 
    (list (length x))
    (screamer::variable (funcallv #'length x))
    (otherwise (error "Cannot take LENGTHV of ~A.~%" x)))))

(defun nthcdrv (n lst)
  (let ((n (value-of n))
        (lst (value-of lst)))
  (assert! (integerpv n))
  (typecase lst
    (list (typecase n 
           (integer (nthcdr n lst))
           (screamer::variable (funcallv #'nthcdr n lst))
            (otherwise (error "Cannot take NTHCDRV ~A of ~A.~%" n lst))))
    (screamer::variable (funcallv #'nthcdr n lst))
    (otherwise (error "Cannot take NTHCDRV ~A of ~A.~%" n lst)))))

(defun make-listv (size &key (initial-element '(make-variable)))
 (if (and (variable? size)
          (not (bound? size)))
     (funcallv #'(lambda (s)
                   (let ((listv '()))
                     (dotimes (c s)
                       (setq listv (nconc listv (list (eval initial-element)))))
                      listv))
                 (value-of size))
    (let ((listv '()))
      (dotimes (c (value-of size))
        (setq listv (nconc listv (list (eval initial-element)))))
      listv)))

(defun mapcarv (function list &rest more-lists)
  "Constraint-propagating MAPCAR for Screamer-Plus."
  (when (variable? function)
    (error "MAPCARV does not allow FUNCTION to be an unbound variable."))
  (let* ((list (value-of list))
         (more-lists (mapcar #'value-of more-lists))
         (z (if (or (variable? list)
                    (some #'variable? more-lists))
                (applyv #'mapcar function (cons list more-lists))
                (apply #'mapcar function (cons list more-lists))))
         (z-len (lengthv z)))
    (cond
      ((and (bound? z-len)
            (every #'bound? (cons list more-lists)))
       (dotimes (i (value-of z-len))
         (assert! (equalv (nthv i z)
                          (apply #'funcallv function
                                 (cons (nth i list)
                                       (mapcar (lambda (lst) (nth i lst)) more-lists))))))
       z)
      ((and (bound? z-len)
            (some (lambda (lst)
                    (and (variable? lst)
                         (known? (listpv lst))))
                  (cons list more-lists)))
       (dotimes (i (value-of z-len))
         (let ((args nil))
           (dolist (lst (cons list more-lists))
             (push (nthv i lst) args))
           (assert! (equalv (nthv i z)
                            (apply #'funcallv function (nreverse args))))))
       z)
      (t z))))

(defun maplistv (function list &rest more-lists)
(when (variable? function)
    (error "The current implementation does not allow the first argument~%~
    of MAPLISTV to be an unbound variable."))
  (let* ((list (value-of list))
         (more-lists (mapcar #'value-of more-lists))
         (z (if (or (variable? list)
                    (some #'variable? more-lists))
                (applyv #'maplist function (cons list more-lists))
                (apply #'maplist function (cons list more-lists))))
         (z-len (lengthv z)))
    (cond
      ((and (bound? z-len)
            (every #'bound? (cons list more-lists)))
       (dotimes (i (value-of z-len))
         (let ((tails (mapcar (lambda (lst)
                                (nthcdr i lst))
                              (cons list more-lists))))
           (assert! (equalv (nthv i z)
                            (apply #'funcallv function tails)))))
       z)
      ((and (bound? z-len)
            (some (lambda (lst)
               (and (variable? lst)
                    (known? (listpv lst))))
             (cons list more-lists)))
       (dotimes (i (value-of z-len))
        (let ((args nil))
          (dolist (lst (cons list more-lists))
            (push (nthcdrv i lst) args))
          (assert! (equalv (nthv i z)
                          (apply #'funcallv function (nreverse args))))))
       
       z)
      (t z))))

(defun listv (&rest elements)
  (let* ((elements (mapcar #'value-of elements))
        (z (applyv #'list elements))
        (z-len (lengthv z)))
  (cond ((bound? z-len)
          (dotimes (i (value-of z-len))
            (assert! (equalv (nthv i z)
                             (nth i elements))))
          z)
        (t z))))

(defun appendv (&rest lists)
  (let* ((z (applyv #'append lists))
         (lengths (if (every #'listp lists)
                      (mapcar #'length lists)
                      (mapcarv #'lengthv lists))))
    (when (and (ground? lengths)
               (deep-bound? lengths)
               (deep-bound? lists))
      (let ((offset 0))
        (dotimes (list-idx (length lists))
          (let ((len (nth list-idx lengths))
                (lst (nth list-idx lists)))
            (dotimes (i (value-of len))
              (assert! (equalv (nthv (+ offset i) z)
                               (nthv i lst))))
            (incf offset (value-of len))))))
    z))

(defun at-mostv (n fn sequence &rest more-sequences)
  (let* ((sequence (value-of sequence))
        (more-sequences (mapcar #'value-of more-sequences))
        (z (a-booleanv)) 
        (count-trues (sumv (mapcarv #'reifyv (apply #'mapv (list* 'list fn sequence more-sequences))))))
    (assert! (impliesv z (<=v count-trues n)))
    (assert! (impliesv (notv z) (>=v count-trues n)))
    (assert!-true z)    
    z))

(defun at-leastv (n fn sequence &rest more-sequences)
  (let* ((sequence (value-of sequence))
        (more-sequences (mapcar #'value-of more-sequences))
        (z (a-booleanv)) 
        (count-trues (sumv (mapcarv #'reifyv (apply #'mapv (list* 'list fn sequence more-sequences))))))
    (assert! (impliesv z (>=v count-trues n)))
    (assert! (impliesv (notv z) (<=v count-trues n)))
    (assert!-true z)
    z))

(defun exactlyv (n fn sequence &rest more-sequences)
  (let* ((sequence (value-of sequence))
        (more-sequences (mapcar #'value-of more-sequences))
        (z (a-booleanv)) 
        (count-trues (sumv (mapcarv #'reifyv (apply #'mapv (list* 'list fn sequence more-sequences))))))
    (assert! (impliesv z (=v count-trues n)))
    (assert! (impliesv (notv z) (/=v count-trues n)))
    (assert!-true z)
    z))
