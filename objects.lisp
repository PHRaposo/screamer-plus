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
  (ifv (andv (slot-boundpv objvar slotname)
             (memberv slotname (slot-names-ofv objvar)))
       (funcallv #'slot-value objvar slotname)
       nil))

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
