;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              ===================================================
;;;              ===                 SCREAMER+                   ===
;;;              ===  increasing the expressiveness of SCREAMER  ===
;;;              ===================================================
;;;
;;;                  Copyright 1998-2000 University of Aberdeen
;;;                  
;;; This source code may be used cost-free for non-commercial use. However,
;;; I request that you provide me with a short description of how you are
;;; using the software so that I can build up a profile of applications.
;;; You are free to modify and extend the source code, but please report any 
;;; bugs found (together with any fixes), so that the quality of the code can
;;; be improved. 
;;;
;;; You may not distribute the code without prior consent from me.
;;;
;;; This software represents "work in progress" and is not (guaranteed to be) 
;;; bug-free!
;;;
;;; And remember...
;;;      'Sometimes a scream is better than a thesis'...
;;;
;;; Happy Screaming!
;;;
;;; Simon White, February 2000                          swhite@csd.abdn.ac.uk
;;;
;;; Department of Computing Science
;;; King's College
;;; University of Aberdeen
;;; Aberdeen AB24 3UE
;;; Scotland, UK.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prevents the production of warnings as a result of loading this patch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+allegro
(setq excl:*redefinition-warnings* 
  (remove :operator excl:*redefinition-warnings*))

(eval-when (:load-toplevel :execute :compile-toplevel)
  (declaim (optimize (speed 1) (safety 3) (space 0) (debug 3))))

(in-package :screamer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This patch enables the search for solutions which are objects
;;; Redefine this function so that 'solution' labels constraint
;;; variables found in objects as well as in conses
;;; value-of has already been applied to x
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(screamer::defmacro-compile-time objectp (var)
  "Determines whether a variable is a standard CLOS object or not"
  `(typep ,var 'standard-object))


(defun variables-in (x &aux slots)
  (typecase x
   (cons             (append (variables-in (value-of (car x))) (variables-in (cdr x))))
   (standard-object  (setq slots (slot-names-of x))
                     (append (variables-in (mapcar #'(lambda(y) (slot-value x y)) slots))))
   (array            (get-array-variables x))
   (variable         (list x))
   (t nil)))


(defun get-array-variables (array)
  "This should collect the values of all cells of a multi-dimensional
  array. Acc is an accumulator for collecting the values of the cells"
  (do* ((dims (array-dimensions array))
         (len (length dims))
         (copydims (make-list len :initial-element 0))
         (acc nil)
         (brand-new t))      
    ((and (every #'zerop copydims) (not brand-new)) acc)

    ;; For some reason SCREAMER is order-sensitive, so I need to use
    ;; append rather than cons
    
    (setq acc (nconc acc (list (apply #'aref (cons array copydims)))))
    (setq copydims (milometer copydims dims))
    (setq brand-new nil)))


(defun milometer (m maxima)
  "This function increments the least significant digit in m If the digit then
  equals the base (maximum) given by the respective element in maxima, it is reset to
  zero and the next significant digit is incremented:
  ;;;  (milometer '(1 2) '(10 10))   => (1 3)
  ;;;  (milometer '(2 3 4) '(5 5 5)) => (2 4 0)
  ;;;  (milometer '(4 4 4) '(5 5 5)) => (0 0 0)"
  (when (null m) (return-from milometer nil))
  (when (not (= (length m) (length maxima)))
    (error "2 lists supplied to milometer must be the same length"))
  (let ((c m))
    (setq c (nconc (butlast c) (list (1+ (car (last c))))))
    (if (= (car (last c)) (car (last maxima)))
      (append (milometer (butlast m) (butlast maxima)) '(0))
      c)))

;;; This function is also used by solution to return the values
;;; found by the search. If objects were explored by the search
;;; NEW instances of the same type are generated and returned.

(defun apply-substitution (x &aux retobj)
  (let ((val (value-of x)))
    (typecase val
      (cons (cons (apply-substitution (car val)) (apply-substitution (cdr val))))
      (standard-object   (setq retobj (make-instance (class-name (class-of val))))
        (copy-slots val retobj)
        retobj)
      (array       (setq retobj (make-array (array-dimensions val)))
                   (copy-cells val retobj)
                   retobj)
      (t val))))

;;; Used by apply-substitution

(defun copy-slots (from to)
  (declare (standard-object from to))
  (dolist (s (slot-names-of from))
     (setf (slot-value to s) (value-of (slot-value from s)))))      

(defun copy-cells (from to)
  (do* ((dims (array-dimensions from))
         (len (length dims))
         (copydims (make-list len :initial-element 0))
         (brand-new t))      
    ((and (every #'zerop copydims) (not brand-new)) to)
    (setf (apply #'aref (cons to copydims)) (value-of (apply #'aref (cons from copydims))))
    (setq copydims (milometer copydims dims))
    (setq brand-new nil)))

;;; This version of funcallv uses ground? to test the boundness of its arguments
;;; instead of bound?

(defun funcallgv (f &rest x)
  (let ((f (value-of f)))
    (if (variable? f)
      (error "The current implementation does not allow the first argument~%~
              of FUNCALLV to be an unbound variable"))
    (unless (functionp f)
      (error "The first argument to FUNCALLV must be a deterministic function"))
    (if (every #'ground? x)
      (apply f (mapcar #'value-of x))
      (let ((z (make-variable)))
        (assert!-constraint #'(lambda (&rest x) (equal (first x) (apply f (rest x)))) t (cons z x))
        (dolist (argument x)
          (attach-noticer! #'(lambda () (if (every #'ground? x)
                                          (assert!-equalv z (apply f (mapcar #'value-of x)))))
            argument))
        z))))

(defun slot-names-of (obj)
 #-sbcl
 (mapcar #'(lambda(x) (slot-value x 'CLOS::NAME))
        (clos::class-slots (class-of obj)))
 #+sbcl
 (mapcar #'sb-mop:slot-definition-name
        (sb-mop:class-slots (class-of obj)))
  )

;(defun slot-names-of (obj)
; (mapcar #'(lambda(x) (slot-value x 'CLOS::NAME))
;  (clos::class-slots (class-of obj))))

;(defun slot-names-of (obj)
;(mapcar #'(lambda (x) (slot-value x 'c2mop::NAME))
 ;(c2mop::class-slots (class-of obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; END OF PATCH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
