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

(in-package :cl-user)

(assert (find-package :SCREAMER))

(defpackage :screamer+
  (:nicknames :?)
  (:use :common-lisp :screamer)
  (:shadowing-import-from :screamer
    :defun :multiple-value-bind :y-or-n-p)
  (:import-from :screamer 
    :known?-true :known?-false :variable-enumerated-domain :variable-enumerated-antidomain
    :assert!-equalv :assert!-constraint :assert!-memberv-internal :variable? :attach-noticer!)
  (:export ;; screamer+
    :listpv :conspv :symbolpv :stringpv :typepv :a-listv :a-consv :a-symbolv
    :a-stringv :a-typed-varv :impliesv :not-equalv :?? :ifv :condv :condv-restricted :make-equal
    :carv :cdrv :consv :firstv :secondv :thirdv :fourthv :restv :nthv :subseqv
    :lengthv :appendv :make-listv :all-differentv :set-equalv :subsetpv
    :intersectionv :unionv :bag-equalv :make-arrayv :arefv :make-instancev :classpv
    :slot-valuev :class-ofv :class-namev  :slot-exists-pv :reconcile
    :funcallinv :mapcarv :maplistv :everyv :somev :noteveryv :notanyv
    :at-leastv :at-mostv :exactlyv :constraint-fn :formatv :*enumeration-limit*
    :carefully :slot-names-of :objectp :eqv :funcallgv :setq-domains)    
  (:export ;; screamer
    :either :local :global :for-effects :local-output :multiple-value-call-nondeterministic
    :nondeterministic-function? :funcall-nondeterministic :apply-nondeterministic :unwind-trail
    :a-boolean :an-integer :a-member-of :an-integer-above :an-integer-below :an-integer-between
    :possibly? :necessarily? :known? :decide  :solution :all-values :one-value :best-value
    :print-values :ith-value  :fail :when-failing :count-failures :boolean :booleanp :assert!
    :make-variable :count-trues :count-truesv :numberpv :realpv :integerpv :booleanpv :memberv
    :=v :<v :<=v :>v :>=v :/=v :+v :-v :*v :/v :minv :maxv :a-booleanv :an-integerv
    :an-integer-abovev :an-integer-belowv :an-integer-betweenv :a-realv :a-real-abovev
    :a-real-belowv :a-real-betweenv :a-numberv :a-member-ofv :notv :andv :orv :funcallv :applyv
    :equalv :bound? :ground? :value-of :apply-substitution :template :domain-size :range-size
    :reorder :static-ordering :linear-force :divide-and-conquer-force :define-screamer-package
    :purge :unwedge-screamer :*dynamic-extent?* :*minimum-shrink-ratio* :*strategy*
    :*maximum-discretization-range* :*screamer-version*))
