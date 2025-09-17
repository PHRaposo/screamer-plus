(in-package :cl-user)

(defpackage :screamer-plus-tests
  (:use :cl :fiveam)
  (:export #:test-screamer-plus #:screamer-plus-tests))

(in-package :screamer-plus-tests)

(def-suite screamer-plus-suite)
(in-suite screamer-plus-suite)

(test test-listpv
    (is (screamer-plus::test-listpv)))

(test test-stringpv
    (is (screamer-plus::test-stringpv)))

(test test-symbolpv
    (is (screamer-plus::test-symbolpv)))

(test test-listv
    (is (screamer-plus::test-listv)))

(test test-stringv
    (is (screamer-plus::test-stringv)))

(test test-symbolv
    (is (screamer-plus::test-symbolv)))

(test test-booleanv-symbolv
    (is (screamer-plus::test-booleanv-symbolv)))

(test test-ifv-1
    (is (screamer-plus::test-ifv-1)))

(test test-ifv-2
    (is (screamer-plus::test-ifv-2)))

(test test-impliesv-1
    (is (screamer-plus::test-impliesv-1)))

(test test-impliesv-2
    (is (screamer-plus::test-impliesv-2)))

(test test-my-memberv-ifv-rec
    (is (screamer-plus::test-my-memberv-ifv-rec)))

(test test-make-equal
    (is (screamer-plus::test-make-equal)))

(test test-condv
    (is (screamer-plus::test-condv)))

(test test-firstv
    (is (screamer-plus::test-firstv)))

(test test-nthv-1
    (is (screamer-plus::test-nthv-1)))

(test test-nthv-2
    (is (screamer-plus::test-nthv-2)))

(test test-subseqv-1
    (is (screamer-plus::test-subseqv-1)))

(test test-subseqv-2
    (is (screamer-plus::test-subseqv-2)))

(test test-lengthv
    (is (screamer-plus::test-lengthv)))

(test test-consv-1
    (is (screamer-plus::test-consv-1)))

(test test-consv-2
    (is (screamer-plus::test-consv-2)))

(test test-carv-1
    (is (screamer-plus::test-carv-1)))

(test test-carv-2
    (is (screamer-plus::test-carv-2)))

(test test-cdrv-1
    (is (screamer-plus::test-cdrv-1)))

(test test-cdrv-2
    (is (screamer-plus::test-cdrv-2)))

(test test-appendv-1
    (is (screamer-plus::test-appendv-1)))

(test test-appendv-2
    (is (screamer-plus::test-appendv-2)))

(test test-make-listv
    (is (screamer-plus::test-make-listv)))

(test test-all-differentv
    (is (screamer-plus::test-all-differentv)))

(test test-set-equalv
    (is (screamer-plus::test-set-equalv)))

(test test-intersectionv-1
    (is (screamer-plus::test-intersectionv-1)))

(test test-unionv-1
    (is (screamer-plus::test-unionv-1)))

(test test-bag-equalv
    (is (screamer-plus::test-bag-equalv)))

(test test-subsetv
    (is (screamer-plus::test-subsetv)))

(test test-a-subset-ofv
    (is (screamer-plus::test-a-subset-ofv)))

(test test-make-arrayv
    (is (screamer-plus::test-make-arrayv)))

(test test-arefv
    (is (screamer-plus::test-arefv)))

(test test-make-instancev
    (is (screamer-plus::test-make-instancev)))

(test test-slot-valuev
    (is (screamer-plus::test-slot-valuev)))

(test test-classpv.1
    (is (screamer-plus::test-classpv.1)))

(test test-classpv.2
    (is (screamer-plus::test-classpv.2)))

(test test-class-ofv
    (is (screamer-plus::test-class-ofv)))

(test test-class-namev
    (is (screamer-plus::test-class-namev)))

(test test-slot-exists-pv
    (is (screamer-plus::test-slot-exists-pv)))

(test test-listv-equalv
    (is (screamer-plus::test-listv-equalv)))

(test test-mapcarv
    (is (screamer-plus::test-mapcarv)))

(test test-maplistv
    (is (screamer-plus::test-maplistv)))

(test test-mapv.1
    (is (screamer-plus::test-mapv.1)))

(test test-mapv.2
    (is (screamer-plus::test-mapv.2)))

(test test-everyv.1
    (is (screamer-plus::test-everyv.1)))

(test test-everyv.2
    (is (screamer-plus::test-everyv.2)))

(test test-somev
    (is (screamer-plus::test-somev)))

(test test-noteveryv
    (is (screamer-plus::test-noteveryv)))

(test test-notanyv
    (is (screamer-plus::test-notanyv)))

(test test-at-leastv
    (is (screamer-plus::test-at-leastv)))

(test test-at-mostv
    (is (screamer-plus::test-at-mostv)))

(test test-exactlyv
    (is (screamer-plus::test-exactlyv)))

(test test-countv
    (is (screamer-plus::test-countv)))

(test test-remove-duplicatesv
    (is (screamer-plus::test-remove-duplicatesv)))

(test test-funcallinv
    (is (screamer-plus::test-funcallinv)))

(test test-formatv
    (is (screamer-plus::test-formatv)))

(defun test-screamer-plus ()
  (run! 'screamer-plus-suite))
