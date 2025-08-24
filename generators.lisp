(in-package :screamer+)
  
(defmacro-compile-time n-variables (n var-fn &rest args)
  "Generate N variables using VAR-FN and ARGS,
  eg. 'an-integer-betweenv 0 10."
  (let ((variables (gensym "VARIABLES")))
   `(let ((,variables nil))
     (dotimes (i ,n)
      (push (apply ,var-fn (list ,@args)) ,variables))
      ,variables)))

(defmacro-compile-time n-lists-of-variables (sizes var-fn &rest args)
  "Generate lists of variables. SIZES is a list, each element is the number of variables in that list.
VAR-FN and ARGS are used to construct each variable, eg. 'an-integer-betweenv 0 10."
  (let ((lists (gensym "LISTS")))
    `(let ((,lists nil))
       (dolist (size ,sizes)
         (let ((vars nil))
           (dotimes (i size)
             (push (apply ,var-fn (list ,@args)) vars))
           (push (nreverse vars) ,lists)))
       (nreverse ,lists))))
