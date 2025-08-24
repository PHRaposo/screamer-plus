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