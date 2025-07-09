**AUCS / TR9805**

**Constraint Handling in**

**Common LISP**

_S. White & D. Sleeman_

Department of Computing Science King’s College University of Aberdeen Aberdeen AB24 3UE Scotland, U.K.

Email: {swhite,dsleeman}@csd.abdn.ac.uk

December 1998

**Abstract**

We demonstrate how constraint programming can be achieved in Common LISP, and share our expe-rience of the Common LISP Constraints Package, SCREAMER. We found the package to be a very use-ful basis for constraint programming in LISP, but were surprised to see that it provides very little support for combining constraints with many typical LISP data structures. We have addressed these shortcomings by providing an additional library of functions, called SCREAMER+. This extension develops the constraint handling package of SCREAMER in three major directions. Firstly, it provides

facilities for expressing and manipulating constraints on LISP lists (including lists interpreted as sets). Secondly, it extends the capabilities for combining constraints with higher order1 functions in LISP

(including logical predicates such as **some**, and **every**). Lastly, it includes functions for dealing with constraints on object-oriented representations in the form of CLOS objects.

**Keywords:** Constraints, LISP, SCREAMER, CLOS.

1\. A higher order function in LISP is a function which accepts another function as an argument.

_Constraint Handling in Common LISP_ _Introduction_

**1** **Introduction**

SCREAMER (Siskind & McAllester, 1993; Siskind & McAllester, 1994) is a freely available extension of Common LISP (Steele, 1990) that provides for _nondeterministic_ and _constraint-based programming_. It was developed at the MIT AI Laboratory and the University of Pennsylvania, and is portable across most modern Common LISP im-plementations. It provides a very useful basis for constraint programming in LISP, but we found it lacking in some important aspects. For example, although SCREAMER ap-pears to provide ample facilities for efficient numeric constraint handling, it is surpris-ing to see that it provides very little support for many usual LISP operations, particularly the manipulation of lists. We have addressed these and other shortcomings by providing an additional library of functions, called SCREAMER+, to extend the original functionality of SCREAMER. This library contains many important new func-tions, including those for expressing and manipulating constraints on LISP lists (in-cluding lists interpreted as sets), some additional higher order constraint functions

(such as a constraint-based version of **mapcar**), and also some functions for dealing

with constraints on CLOS1 objects.

In the following section, we sketch the features of SCREAMER, and comment on our experiences of using it. We explain how SCREAMER integrates nondeterminism into LISP and summarise the facilities provided (and not provided) by the constraints pack-age. Section 3 addresses perceived shortcomings of SCREAMER by describing our ex-tension, SCREAMER+. Section 4 describes two example applications which use some of the functions introduced by SCREAMER+. Finally, section 5 summarises our achievements and provides some brief concluding remarks.

**2** **The Features of SCREAMER**

**2.1 Nondeterminism using SCREAMER**

To add nondeterminism to LISP, SCREAMER defines a _choice point_ operator and a _fail_ operator. Choice points are most easily introduced with the macro **either**, and fail-ures are generated with the function **fail**. Functions and other LISP expressions which define choice points but do not attempt to retrieve values from them are called _nondeterministic contexts_. A small number of new functions and macros are also sup-plied by SCREAMER for retrieving values from nondeterministic contexts, notably **one-value**and **all-values**. Let us now illustrate these ideas with the following simple example:

```
(one-value (either 'black 'blue))
```
BLACK

```
(one-value (list (either 'tall 'short) (either 'fat 'thin)))
```
(TALL FAT)

```
(all-values (list (either 'tall 'short) (either 'fat 'thin)))
```
((TALL FAT) (TALL THIN) (SHORT FAT) (SHORT THIN))

**Either**can be called with any number of expressions as arguments, and initially re-turns the value of the first supplied expression to the surrounding nondeterministic context. If a failure later causes a backtrack to this choice point, the next expression will be chosen and returned. If SCREAMER backtracks to a choice point which has no further values, then backtracking continues to the previous choice point. If no choice points remain, an error is generated.

**Either**also forms the basis of several other more specialised nondeterministic forms. For example, consider defining a function **an-integer-between**, which nondeter-ministically returns integers between two supplied integers _low_ and _high_:

```
(defun an-integer-between (low high)

(when (or (not (integerp low)) (> low high)) (fail)) (either low (an-integer-between (1+ low) high))

)
```

The first line of this definition traps unexpected parameter values and halts recursion; the second line nondeterministically returns either the integer _low_ itself, or, should that fail, an integer between _1+low_ and _high_. A similar function can be defined to nonde-terministically return the members of a list. SCREAMER already provides many of the commonly used nondeterministic functions, as well as some macros for protecting LISP variables from any local side-effects caused by backtracking.

Nondeterministic programming using SCREAMER is exemplified by a solution2 to the N-Queens problem, as given in Figure 1.

**2.2 Constraint Handling using SCREAMER**

The SCREAMER constraints package provides LISP functions which enable a pro-grammer to _create constraint variables_, (often referred to simply as _variables_), _assert constraints_ on those variables, and _search for assignments of values to variables_ ac-cording to the asserted constraints.

An unbound constraint variable, _x_, can be created with a call to the function **make-variable**; for example: **(setq x (make-variable))**. Assertions are carried out using _constraint primitives_, which are generally constraint-based counterparts of some Common LISP function. So, for example, **integerpv**is the SCREAMER constraint primitive corresponding to the LISP function**integerp**, which determines whether its argument is an integer. It is a convention that constraint primitives end in the letter v,

2\. The code is adapted from (Siskind and McAllester, 1993).

_2_

_Constraint Handling in Common LISP_ _The Features of SCREAMER_

```
(defun attacks-p (qi qj distance) (or (= qi qj)

(= (abs (- qi qj)) distance)))

(defun check-queens (queen queens &optional (distance 1)) (unless (null queens)

(if (attacks-p queen (first queens) distance) (fail)) (check-queens queen (rest queens) (1+ distance))))

(defun n-queens (n &optional queens) (if (= (length queens) n)

queens

(let ((queen (an-integer-between 1 n))) (check-queens queen queens)

(n-queens n (cons queen queens)))))

(defun queens (n)

(dolist (sol (one-value (n-queens n))) (dotimes (c n)

(format t " ~a" (if (= (1- sol) c) "Q" "+"))) (terpri)

) )

```

**Figure 1: A Solution to the N-Queens Problem using nondeterministic programming in SCREAMER**

because each of them creates and returns a constraint variable to hold the result of eval-uating the expression. To assert truths about constraint variables, one uses the primitive **assert!**, which takes an expression as its argument and binds the constraint variable associated with that expression to true. Consider the expression **(assert! (inte-gerpv x))**, which constrains _x_ to be an integer. Likewise, the expression **(assert! (andv (>=v x 0) (<=v x 10)))**sets the inclusive range of _x_ to be between 0 and 10. Once the problem has been set up in this way, solutions can be found with the non-deterministic function **solution**. This function explores the search space of domain values, returning each solution found to its surrounding nondeterministic context. Two arguments must be supplied to **solution**: firstly a data structure (typically a list) con-taining those constraint variables requiring assignments, and secondly, a function for ordering the variables at each choice point. The simplest of these is a static-ordering which linearly forces the constraint variables to assume each value in its domain in turn, until a solution is found. So, to find all possible values of x in the above example, one uses **(all-values (solution x (static-ordering #’linear-force)))**.

**2.3 Discussion**

We were impressed with the way that SCREAMER integrates nondeterminism and con-straint-handling into Common LISP. It appears to be very good for solving sets of log-ical and/or numeric constraints; indeed, it is superior to many other constraint solvers in its ability to deal with sets of non-linear constraints. We were surprised, however, at

_3_

_Constraint Handling in Common LISP_ _The Features of SCREAMER_

the lack of facilities for symbolic constraint handling, such as the expression of con-straints on lists. Two of the major features of Common LISP are its list handling and its provision of higher-order functions (functions which take functions as arguments), such as **mapcar**. We therefore felt that in order to maximise the utility of constraints in LISP, we should extend SCREAMER to embrace those qualities. In addition, we felt the ability to impose constraints on Common LISP objects would be useful for more sophisticated data/knowledge modelling problems. The functions of our extension to SCREAMER, SCREAMER+, have been designed to provide the required functionality for these three main directions.

To assess the scope of SCREAMER and its extension, SCREAMER+, refer to tables 1 and 2. Table 1 summarises the constraint primitives provided by SCREAMER, and ta-ble 2 summarises the additional primitives provided by SCREAMER+. The primitives of SCREAMER are documented in (Siskind, 1991); those of SCREAMER+ are de-scribed in the following section of this document.

Type Restrictions:

Boolean:

Numeric:

Expression:

Function:

**numberpv realpv integerpv booleanpv memberv**

**andv orv notv**

**&lt;v <=v &gt;v >=v =v /=v +v -v \*v /v minv maxv**

**equalv**

**funcallv applyv**

**Table 1: The Constraint Primitives of SCREAMER**

We believe that SCREAMER is difficult for seasoned LISP programmers to grasp ini-tially, because its programs tend to be a mixture of nondeterministic and/or constraint-based programming, and conventional LISP functions. At first, we found it easier to

write nondeterministic programs than constraint-based ones. However, we believe that

the efficient search procedure3 of the constraints package combined with its superior ability to deal with infinite domains makes it a more attractive approach for most ap-plications. For this reason, we have only extended the constraints package of SCREAMER; the underlying nondeterminism of SCREAMER remains unchanged.

Another advantage of the constraints package is that it lends itself to an interactive ses-sion, because it is easy to inspect intermediate results. For example, this is what hap-pens when creating a constraint variable:

```
(setq x (make-variable))
```
\[72\]

3\. The constraints package of SCREAMER includes five kinds of inference process to improve its search performance over naive backtracking. The inference processes are: binding propagation, Boolean con-straint propagation (BCP), generalised forward checking (GFC), bounds propagation on numeric varia-bles, and unification.

_4_

_Constraint Handling in Common LISP_ _The Features of SCREAMER_

```
(assert! (integerpv x))
```
NIL

```
x
```
\[72 integer\]

; assert! always returns nil ; Check the value of x

Now when we inspect the value of the variable, SCREAMER reminds us that it is con-strained to be an integer. When we constrain the integer values that it can take, this in-formation is also made visible:

```
(assert! (andv (>=v x 0) (<=v x 10)))
```
NIL

```
x
```
\[72 integer 0:10 enumerated-domain:(0 1 2 3 4 5 6 7 8 9 10)\]

_5_

_Constraint Handling in Common LISP_ _The Features of SCREAMER_

The entry 0:10 in the returned value indicates the lower and upper bounds of the range of values which the variable can take, and the_enumerated domain_represents the_actual_ values which it can take within the given range. Feedback such as this is not only re-assuring when things are going well, it also provides invaluable debugging information when things are not working as expected.

Not all constraints have an immediate effect on the domain values of variables, how-ever. Sometimes, the condition associated with the constraint is wrapped up into a small body of executable code (implemented as a lambda function) and stored as a so-called _noticer_. Noticers are used later, for example, when further assertions are made on the variable, but often not until the associated variable becomes bound as part of the search process. As an example of this, consider a further assertion on the example we have already developed:

```
(assert! (notv (=v x 2)))
```
NIL

```
x ; Check the value of x
```
\[72 integer 0:10 enumerated-domain:(0 1 2 3 4 5 6 7 8 9 10)\]

The assertion appears not to have reduced the domain of the variable. But the search for solutions reveals that the constraint has not been forgotten:

```
(all-values (solution x (static-ordering #'linear-force)))
```
(0 1 3 4 5 6 7 8 9 10)

Nevertheless, we feel that since the inspection of intermediate values is such an advan-

tage, the propagation of constraints to domain values should occur as early as possi-

ble4. This may also enable some reasoning about the value of the solution even before the search process starts. In SCREAMER+, therefore, we have attempted to propagate values as early as practicable.

Unfortunately, SCREAMER lacks the ability to _retract_ constraints, so that some previ-ous problem solving state can be resumed. This is important because the assertion of a

constraint which ultimately fails can irreparably damage the values of constraint vari-

ables5. We found that this problem can be circumvented either by taking copies6 of constraint variable structures _before_ dangerous assertions are made, or checking first that the assertion does not fail by using the SCREAMER macro **possibly?**.

We found that in some cases, SCREAMER had not carried some of the finer detail of LISP through to its constraint-based counterparts. Most importantly, the SCREAMER function **equalv**had not been defined in terms of the LISP function **equal**, but in terms of the function **eq**instead! This led to some unexpected results; for example, **(equalv “foo” “foo”)** returned **nil**. This probably reflects the fact that SCREAMER was originally designed for solving constraint problems with _atomic_ (e.g. numbers, Boolean values), rather than _symbolic_ solutions (such as lists). In our copy of SCREAMER, we have changed the definition of **equalv**so that it does mirror the LISP definition of **equal**.

4\. As long as the propagation does not impinge severely on efficiency.

5\. This does not apply to nondeterministic failure within the scope of a call to **solution**.

6\. Constraint variables are stored in LISP structures, so a temporary copy of its state can be made by the LISP function **copy-structure**

_6_

_Constraint Handling in Common LISP_ _The Extension to SCREAMER_

Similarly, we noticed that **memberv**does not accept a keyword _test_ argument, like the Common LISP function **member**. (Recall that **member**tests for the equality of some value against each of the members of some list using a test function, which can be sup-plied as an argument, but is **eq**by default). Thus, it is not possible in SCREAMER to constrain a variable to be one of, say, three candidate lists. In our current version of SCREAMER+, we have changed the definition of **memberv**so that it always uses the test **equal**instead of **eq**. This is sufficient to allow us to work with compound struc-tures such as lists.

As a final observation, we found that the SCREAMER function (**funcallv**_f x1 ... xn_)

makes an_idempotency assumption_of its function argument. That is, rather than call the supplied function just once as soon as the other arguments became bound, the function was called many times! Often, this is not too detrimental, impinging only on efficiency and not on correctness. On the occasions when the function_f_has side-effects, however, the difference can be crucial. The problem is simple to illustrate with the following ex-ample:

```
(setq a (make-variable)) \[1\]

(funcallv #'print a) ; Print a when it becomes bound \[2\]

(assert! (equalv a 'hello)) HELLO

HELLO HELLO NIL

As soon as the constraint variable became bound, the message “HELLO” was output not once, but three times! Note that this particular problem has been addressed by the function**formatv**in SCREAMER+, but the idempotency assumption of**funcallv**re-mains.

**3** **The Extension to SCREAMER**

**3.1 Type Restrictions**

**listpv, conspv, symbolpv, stringpv** \[Macros\]

**Synopsis:** (**listpv_X_**) **|** (**conspv_X_**) **|** (**symbolpv_X_**) **|** (**stringpv**_X_)

**Description:** The macros **listpv**, **conspv**, **symbolpv**, and **stringpv**each return a variable, _z,_ constrained to indicate with a boolean value whether _x_ is of the given type. For example, the variable returned by **listpv**indicates whether its argument is a list as soon as the argument itself becomes bound. Each of these macros is a specialisation of the **typepv** function.

_7_

_Constraint Handling in Common LISP_ _The Extension to SCREAMER_

**Examples**

;;; Create a constraint variable > (setq x (make-variable))

\[432\]

;;; Constrain z to be the listp of x > (setq z (listpv x))

\[433\]

;;; Bind x to a non-list value > (make-equal x "hello") "hello"

;;; Inspect the value of z > z

NIL

;;; Constrain a to be one of the given values > (setq a (a-member-ofv '(1 nil t (hello)))) \[124 enumerated-domain:(1 NIL T (HELLO))\]

;;; Assert a to be a list > (assert! (listpv a)) NIL

;;; The possible values are now only those which are lists > a

\[124 nonnumber enumerated-domain:(NIL (HELLO))\]

**typepv** \[Function\]

**Synopsis:** (**typepv**_x type_)

**Description:** This function returns a variable, _z,_ constrained to indicate with a boolean value whether _x_ is of the given type. The supplied _type_ must be one of those acceptable to the Common LISP _typep_ function. The output variable _z_ does not become bound un-til both _x_ and _type_ are bound. However, if _z_ is bound to true, _type_ is bound, and _x_ has an enumerated domain, then all values are eliminated from the domain which are not of the supplied type.

**Examples**

```
(setq x (make-variable))
```
\[434\]

```
(setq ty (make-variable))
```
\[435\]

```
(setq z (typepv x ty))
```
\[436\]

```
(make-equal x "jolly")
```
"jolly"

```
(make-equal ty 'string)
```
STRING

```
z
```
T

_8_

_Constraint Handling in Common LISP_

**a-listv, a-consv, a-symbolv, a-stringv**

_The Extension to SCREAMER_

\[Macros\]

**Synopsis:** (**a-listv**) **|** (**a-consv**) **|** (**a-symbolv**) **|** (**a-stringv**)

**Description:**The macros**a-listv**,**a-consv**,**a-symbolv**and**a-stringv**are com-monly used specialisations of the function **a-typed-varv**, which are used to con-strain a variable to be either a list, cons, symbol, or string, respectively. See also **a-typed-varv**.

**Examples**

```
(setq z (a-listv))
```
\[100\]

```lisp
(make-equal z 'not-a-list nil)
```
Warning: (make-equal Z 'NOT-A-LIST) failed Z = \[100\]; 'NOT-A-LIST = NOT-A-LIST

NIL

**a-typed-varv** \[Function\]

**Synopsis:** (**a-typed-varv**_type_)

**Description:** This function returns a variable, _z_, constrained to be of the given LISP type. An attempt to bind _z_ to a value which is incompatible with the supplied type will fail.

**Examples**

```lisp
(setq z (a-typed-varv 'string))
```
\[97\]

```lisp
(make-equal z '(1 2) 'failed)
```
Warning: (make-equal Z '(1 2)) failed

Z = \[97\]; '(1 2) = (1 2) FAILED

```lisp
(setq a (a-typed-varv 'string))
```
\[98\]

```lisp
(assert! (memberv a '(1 two "three")))
```
NIL

```lisp
a
```
"three"

_9_

_Constraint Handling in Common LISP_ _The Extension to SCREAMER_

**3.2 Boolean Values**

**impliesv** \[Function\]

**Synopsis:** (**impliesv** _x y_)

**Description:** This function is a simple extension to SCREAMER which returns a boolean variable, _z_, constrained to indicate whether _x_ implies _y_. Typically, the impli-cation is asserted, and _y_ becomes bound to true as soon as _x_ becomes true.

;;; Create two boolean constraint variables, x and y > (setq x (a-booleanv))

\[360 Boolean\]

\> (setq y (a-booleanv)) \[361 Boolean\]

;;; Assert that x implies y > (assert! (impliesv x y)) NIL

;;; Set x to be true > (make-equal x t)

T

;;; Check that y is bound to true > y

T

;;; The truth table for implies is easy to generate: > (setq p (a-booleanv))

\[3558 Boolean\]

\> (setq q (a-booleanv)) \[3559 Boolean\]

\> (setq r (a-booleanv)) \[3560 Boolean\]

\> (assert! (equalv r (impliesv p q))) NIL

\> (all-values (solution (list p '=> q 'is r) (static-ordering #'linear-force)))

((T => T IS T) (T => NIL IS NIL) (NIL => T IS T) (NIL => NIL IS T))

**3.3 Expressions**

**ifv** \[Macro\]

**Synopsis:** (**ifv** _condition expression1 expression2_)

**Description:** This macro returns a variable, _z_, constrained to be the value of _expression1_ or _expression2_. If the boolean constraint variable _condition_ becomes bound to **t**, then _z_ becomes bound to _expression1_; otherwise _z_ becomes bound to _expression2_as soon as_condition_becomes bound to**nil**. An important property of**ifv**

_10_

_Constraint Handling in Common LISP_ _The Extension to SCREAMER_

is that neither _expression1_ nor _expression2_ are actually evaluated until the _condition_ becomes bound. This means that the macro can be used in recursive constraint defini-tions, such as that given below.

**Examples**

;;; Create our own recursive definition of memberv > (defun my-memberv (m ll)

(when ll

(ifv (equalv m (carv ll)) t

(my-memberv m (cdrv ll)) )

) )

MY-MEMBERV

```lisp
(setq x (make-variable))
```
\[11731\]

```lisp
(setq z (my-memberv 1 x))
```
\[11741\]

```lisp
(make-equal x '(3 2 1))
```
(3 2 1)

```lisp
z
```
T

**make-equal** \[Macro\]

**Synopsis:** (**make-equal**_x y_ \[_failure-expression_\] )

**Description:** This macro attempts to constrain _x_ to be **equal**to _y._ Although the argu-ments are actually symmetric, _x_ is usually a constraint variable, and _y_ some new (con-strained) value, so that the macro amounts to the equivalent of an assignment in imperative language programming. The macro returns the updated value of _x_ if the as-sertion succeeded, which is now constrained to be **equal**to _y_. If the assertion fails, then the _failure-expression_ is evaluated and returned. Also, unlike a conventional SCREAMER **(assert! (equalv x y))**, a failed **make-equal**assertion leaves _x_ with the same value after the assertion as it had before the assertion was attempted.

**Examples**

;;; First, an example of make-equal succeeding... > (setq x (make-variable))

\[4\]

```lisp
(make-equal x '(1 2 3))
```
(1 2 3)

```lisp
x
```
(1 2 3)

;;; Now an example of the assertion failing... > (setq a (an-integer-betweenv 1 5))

\[10214 integer 1:5 enumerated-domain:(1 2 3 4 5)\] ```lisp
(make-equal a 9 nil)
```
Warning: (make-equal A 9) failed

A = \[10214 integer 1:5 enumerated-domain:(1 2 3 4 5)\]; 9 = 9 NIL

```lisp
a
```
\[10214 integer 1:5 enumerated-domain:(1 2 3 4 5)\] >

**3.4 Lists and Sequences**

**firstv, secondv, thirdv, fourthv** \[Macros\]

**Synopsis:** (**firstv** _x_) **|** (**secondv** _x_) **|** (**thirdv** _x_) **|** (**fourthv**_x_)

**Description:** The functions **firstv**, **secondv**, **thirdv**, and **fourthv**each return a variable, _z,_ constrained to be either the first, second, third, or fourth element of a list, respectively. If_x_is already bound to a list value at the time of invocation, then the_value_ of the constraint variable _z_ is returned, rather than the variable itself. Otherwise _x_ must be a constraint variable, and _z_ becomes bound as soon as _x_ becomes bound. If _x_ be-comes bound to a value which is not a list, so that the core function **first**, **second**, **third**, or **fourth**cannot be properly executed, then a failure is generated. Note that **firstv**is identical to **carv**, except that **firstv**is implemented as a macro and **carv** is implemented as a function.

**Examples**

;;; If the argument is bound the answer is returned directly > (firstv '(a b c))

A

;;; Create an unbound constraint variable, x > (setq x (make-variable))

\[813\]

;;; Constrain z to be the first element of x > (setq z (firstv x))

\[814\]

;;; Bind x to a specific value > (make-equal x '(a b c))

(A B C)

;;; Inspect z > z

A

_12_

_Constraint Handling in Common LISP_

**nthv**

_The Extension to SCREAMER_

\[Function\]

**Synopsis:** (**nthv**_n x_)

**Description:** The function **nthv**returns a variable, _z,_ constrained to be the _n_th element of the list_x._Both_n_and_x_may be either a bound value or an unbound constraint variable at the time of function invocation. The argument _n,_ when bound, is an integer which can range from 0, which indexes the first element of the list, to (length _x_) - 1, which indexes the last element_._ As soon as _x_ becomes bound, the value of _z_ is computed. Sim-

ilarly, if _z_ and _n_ both become bound before _x_, then _x_ is constrained (but not bound) to

be a list whose _n_th element is _z_. If the bindings are such that the index _n_ is out of range for the list _x_, or _x_ is not a list at all, a failure is generated.

**Examples**

;;; This example extracts the nth element of a list > (setq x (make-variable))

\[823\]

```lisp
(setq z (nthv 2 x))
```
\[824\]

```lisp
(make-equal x '(a b c d e f))
```
(A B C D E F)

```lisp
z
```
C

;;; This example sets the nth element of a list > (setq a (make-listv 4))

(\[396\] \[395\] \[394\] \[393\])

```lisp
(assert! (equalv (nthv 1 a) 'foo))
```
NIL

```lisp
a
```
(\[413\] FOO \[394\] \[393\]) > (secondv a)

FOO

**subseqv** \[Function\]

**Synopsis:** (**subseqv**_x start_ \[_stop_\] )

**Description:** The function **subseqv**returns a variable, _z_, constrained to be the subse-quence of _x_ running from the inclusive index _start_ up to the exclusive index _stop_. (The index of the first value in a sequence is zero.) If _stop_ is not supplied, the subsequence _z_runs from_start_to the end of_x_. The variable_z_becomes bound as soon as its arguments become bound. Also, if _z_ should become bound to a list value, for example before the elements of _x_ are all bound, then the elements of _z_ are unified with their respective el-ements of _x_.

_13_

_Constraint Handling in Common LISP_ _The Extension to SCREAMER_

**Examples**

;;; When the args are bound, subseqv is the same as subseq > (subseqv '(1 2 3 4 5 6) 2 4)

(3 4)

;;; Create a constraint variable for the index > (setq n (make-variable))

\[199\]

;;; Constrain a to be a subsequence of ‘(1 2 3 4 5 6) > (setq a (subseqv '(1 2 3 4 5 6) n))

\[201\]

;;; Bind n

```lisp
(make-equal n 3)
```
3

;;; Check the value of a > a

(4 5 6)

;;; Create a list of 6 elements > (setq x (make-listv 6))

(\[203\] \[204\] \[205\] \[206\] \[207\] \[208\]) ;;; Create another list of 2 elements > (setq z (make-listv 2))

(\[210\] \[211\])

;;; Say that the elements of z are both integers > (assert! (integerpv (firstv z))

) NIL

```lisp
(assert! (integerpv (secondv z))) NIL
```
```lisp
z
```
(\[210 integer\] \[211 integer\])

;;; Assert that z are the same as the middle two of x > (assert! (equalv z (subseqv x 2 4)))

NIL

;;; Check that the elements of z have been unified with x > x

(\[203\] \[204\] \[205 integer\] \[206 integer\] \[207\] \[208\]) >

**lengthv** \[Function\]

**Synopsis:** (**lengthv**_x_ \[_is-list_\])

**Description:** The function **lengthv**returns a variable, _z_, constrained to be the length of the list _x_. If _x_ is already bound at the time of function invocation, then the length of _x_ is returned directly. Otherwise _z_ becomes bound as soon as _x_ becomes bound. Al-though **lengthv**will work with other types of sequences, such as strings and one-di-menional arrays, its propagation properties have been optimised for use with lists. If _z_ should become bound before _x_ and the optional argument _is-list_ is not supplied (or is supplied with a non-**nil**value), then _x_ is assumed to be a list and becomes bound to a list structure of the length given by _z_. If _z_ becomes bound before _x_, and _is-list_ is sup-

_14_

_Constraint Handling in Common LISP_ _The Extension to SCREAMER_

plied with the value **nil**, then no assumptions are made about the sequence type of _x_, but no propagation takes place from _z_ to _x_ either. Also, if _x_ has an enumerated domain of possible values, then the lengths of these sequences are propagated through to _z_.

**Examples**

```lisp
(setq a (a-listv))
```
\[104\]

```lisp
(setq b (lengthv a))
```
\[107\]

```lisp
(make-equal b 4)
```
4

```lisp
a
```
(\[112\] \[111\] \[110\] \[109\])

;;; Create a constraint variable called x > (setq x (make-variable))

\[185\]

;;; Constrain len to be the length of x, where x is not a list > (setq len (lengthv x nil))

\[187 integer\]

;;; Enumerate the possibilities for x

```lisp
(assert! (memberv x '("one" "two" "three" "four")))
```

NIL

;;; Check the domain of x > x

\[185 nonnumber enumerated-domain:("one" "two" "three" "four")\] ;;; Check the domain of len

```lisp
len
```
\[187 integer 3:5 enumerated-domain:(3 5 4)\] ;;; Bind x

```lisp
(make-equal x "three")
```
"three"

;;; Check that len has also been bound > len

5

**consv** \[Function\]

**Synopsis:** (**consv**_x y_)

**Description:** The function **consv**returns a variable, _z,_ constrained to be the **cons**of _x_ and _y_. If _y_ is bound at the time of function invocation then _z_ is immediately bound to

the **cons**of _x_ and _y_ (regardless of whether _x_ is bound); otherwise _z_ becomes bound to

the **cons**of _x_ and _y_ as soon as _y_ becomes bound7. If _z_ should become bound before _y_, then _x_ and _y_ become bound to the **car** and **cdr** of _z_, respectively.

7\. We should also like to bind _z_ if _x_ is bound and _y_ is unbound, so that (car z)would return _x_. The problem is that if the tail of _z_ becomes bound to a constraint variable because _y_ is not bound, then LISP stores _z_ as a dotted list pair whose cdris an unbound constraint variable (rather than a list). This dotted list pair cannot later be unified with a list when _y_ eventually becomes bound.

**Examples**

;;; Create a constraint variable > (setq x (make-variable))

\[1\]

;;; Constrain z to be the cons of the symbol ‘g and x > (setq z (consv 'g x))

\[2\]

;;; Bind x to a value

```lisp
(make-equal x '(1 2 3)) (1 2 3)

;;; Inspect the value of z > z

(G 1 2 3)

;;; Create two constraint variables to be cons’ed > (setq x (make-variable))

\[7\]

```lisp
(setq y (make-variable)) \[8\]

;;; Constrain z to be the cons of x and y > (setq z (consv x y))

\[9\]

;;; Bind the value of z

```lisp
(make-equal z '(a b c)) (A B C)

;;; Check that the values of x and y have been derived > x

A

```lisp
y
```
(B C)

**carv** \[Function\]

**Synopsis:** (**carv**_x_)

**Description:** The function **carv**returns a variable, _z,_ constrained to be the _car_ (i.e. first element) of a _cons_. (A cons is either a normal list or a dotted list.) If _x_ is already bound to a cons value at the time of invocation, then the_value_of the constraint variable _z_ is returned, rather than the variable itself. Otherwise _x_ must be a constraint variable, and _z_ becomes bound as soon as _x_ becomes bound. If _z_ becomes bound before _x_, then _x_ is constrained to be a cons in which _z_ is the first element. If _x_ becomes bound to a value which is not a cons, so that the Common LISP function _car_ cannot be properly executed, then a fail is generated.

**Examples**

;;; Create a variable to contain a list > (setq x (make-variable))

\[13\]

;;; Constrain the variable z to be the car of x > (setq z (carv x))

_16_

_Constraint Handling in Common LISP_ _The Extension to SCREAMER_

```lisp
(make-equal x '(fee fi fo fum)) (FEE FI FO FUM)

;;; Inspect the value of z > z

FEE

;;; Create a variable to contain a dotted pair > (setq x (make-variable))

\[153\]

;;; Constrain z to be the first element of the pair > (setq z (carv x))

\[154\]

;;; Create a binding for x

```lisp
(make-equal x (cons 'one 'two)) (ONE . TWO)

;;; Inspect the value of z > z

ONE

See also the examples for **cdrv**.

**cdrv** \[Function\]

**Synopsis:** (**cdrv**_x_)

**Description:** The function **cdrv**returns a variable, _z,_ constrained to be the _cdr_ (i.e. the tail) of a _cons_ (normally a list). If _x_ is already bound to a cons at the time of invocation, then the _value_ of the constraint variable _z_ is returned, rather than the variable itself. Otherwise _x_ must be a constraint variable, and _z_ becomes bound as soon as _x_ becomes bound. If _z_ becomes bound before _x_, then _x_ is constrained to be a cons which has _z_ as its _cdr_. If _x_ becomes bound to a value which is not a _cons_, so that the Common LISP function _cdr_ cannot be properly executed, then a fail is generated.

**Examples**

;;; Create a constraint variable to contain a list > (setq x (make-variable))

\[133\]

;;; Constrain z to be the tail of the list > (setq z (cdrv x))

\[134\]

;;; Create a binding for x > (make-equal x '(1 2 3 4)) (1 2 3 4)

Inspect the value of z > z

(2 3 4)

\> (setq x (make-variable)) \[114\]

\> (setq head (carv x))

\[115\]

\> (setq tail (cdrv x)) \[116\]

```lisp
(make-equal head 'g) G

```lisp
(make-equal tail '(h i)) (H I)

```lisp
x
```

(G H I)

**appendv** \[Function\]

**Synopsis:** (**appendv**_x1 x2_ \[ _... xn_ \] )

**Description:** The function **appendv**returns a variable, _z,_ constrained to be the **ap-pend**of all its arguments, unless all the arguments are bound at the time of function invocation, when the value of the append is returned directly. The function applies tail

recursion to any arguments _x3 ... xn_ supplied. Usually, however, **appendv**will be called with only two arguments, so that _z_ is constrained to be the **append**of _x1_ and _x2_. In such circumstances, when any two of _x1_, _x2_ and _z_ become bound, the third value is deduced. If _z_ only should become bound, then _x1_ and _x2_ are each constrained to be one

of the sublists taken from the front or back of _z_, respectively. For example, if the ap-pend,_z_, of two lists_x_and_y_is known to be**’(a b c)**, then_x_must be one of**’()**,**’(a)**, **’(a b)**, and **’(a b c)**, and _y_ must be one of **’(a b c)**, **’(b c)**, **’(c)**, and **’()**.

**Examples**

;;; Create a constraint variable > (setq x (make-variable))

\[165\]

;;; Constrain z to be the append of x and ‘(3 4) > (setq z (appendv x '(3 4)))

\[166\]

```lisp
(bind x '(1 2))
```
(1 2)

```lisp
z
```
(1 2 3 4)

;;; Create two constraint variables > (setq x (make-variable))

\[167\]

\> (setq y (make-variable)) \[168\]

;;; Assert that appending x to y gives the result ‘(a b c) > (assert! (equalv (appendv x y) '(a b c)))

NIL

;;; Inspect x and y - note the enumerated domains > x

\[167 nonnumber enumerated-domain:(NIL (A) (A B) (A B C))\] > y

\[168 nonnumber enumerated-domain:((A B C) (B C) (C) NIL)\] ;;; Bind x to one of its possible values

```lisp
(make-equal x '(a))
```
(A)

;;; Inspect y, which has now automatically been bound > y

(B C)

**make-listv** \[Function\]

**Synopsis:** (**make-listv**_n_)

**Description:** The function **make-listv**returns a variable which is constrained to be a list of the length given by _n_. More precisely, the function actually generates a list of length_n_with a different constraint variable taking the place of every element. Note that since a constraint variable is generated for every place in the list, there is a memory overhead in generating large lists using this function. For long lists it is preferable to use **a-listv**which does not actually construct a list, but instead checks that the var-iable is a list once it is instantiated.

**Examples**

```lisp
(setq n (make-variable))
```
\[28\]

```lisp
(setq x (make-listv n))
```
\[29\]

```lisp
(make-equal n 4)
```
4

```lisp
x
```
(\[33\] \[32\] \[31\] \[30\])

```lisp
(assert! (equalv (nthv 1 x) 'foo))
```
NIL

```lisp
x
```
(\[36\] FOO \[31\] \[30\]) >

**all-differentv** \[Function\]

**Synopsis:** (**all-differentv**_x1 x2 ... xn_)

**Description:** This function returns a boolean variable, _z_, constrained to indicate whether all the supplied arguments have distinct values. This is in effect a symbolic equivalent of the existing numeric SCREAMER constraint predicate ‘**/=v**’, which can be used to constrain its numeric arguments to contain distinct values.

Thus, whilst **(assert! (/=v**_x1 x2 x3_**))** is equivalent to: **(andv (numberpv** _x1_**) (numberpv** _x2_**) (numberpv** _x3_**)**

**(notv (=v**_x1 x2_**)) (notv (=v**_x2 x3_**)) (notv**

**(=v**_x1 x3_**)))**,

_19_

_Constraint Handling in Common LISP_ _The Extension to SCREAMER_

the symbolic counterpart **(assert! (all-differentv** _x1 x2 x3_**))**is equivalent to

**(andv(notv(equalv**_x1 x2_**))(notv(equalv**_x2 x3_**))(notv(equalv**_x1 x3_**)))**. **Examples**

;;; Clearly all distinct

```lisp
(all-differentv 'a 'b 'c)
```
T

;;; Not yet clear whether the values are distinct > (all-differentv (a-member-ofv '(a b)) 'b 'c) \[10224 Boolean\]

;;; Clearly not all distinct > (all-differentv 'a 'b 'a) NIL

**3.5 Sets and Bags**

**set-equalv** \[Function\]

**Synopsis:** (**set-equalv** _x y_)

**Description:** The function **set-equalv**returns a boolean variable, _z_, which is con-strained to indicate whether its two list arguments are equal if they are interpreted as sets. In other words, as soon as both _x_ and _y_ become bound, _z_ becomes bound to true if _x_ and _y_ have the same members; otherwise _z_ becomes bound to false.

**Examples**

```lisp
(setq x '(a b c))
```
(A B C)

```lisp
(setq y (make-variable))
```
\[283\]

```lisp
(setq same-set (set-equalv x y))
```
\[284 Boolean\]

```lisp
(make-equal y '(b b a c a a))
```
(B B A C A A)

```lisp
same-set
```
T

;;; In this example the second set has 4 distinct members > (set-equalv '(a b c) '(b b a c a d))

NIL

_20_

_Constraint Handling in Common LISP_

**intersectionv**

_The Extension to SCREAMER_

\[Function\]

**Synopsis:** (**intersectionv** _x y_)

**Description:** The function **intersectionv**returns a variable, _z_, constrained to be the intersection of the two sets _x_ and _y_. The output variable _z_ becomes bound as soon

as both _x_ and _y_ become bound. If either _x_ or _y_ has duplicate entries, the redundant en-

tries may or may not appear in the result8. Otherwise, if _z_ becomes bound before _x_ or _y_, then the latter are constrained to contain each of the elements of _z_.

**Examples**

;;; Create a variable x

```lisp
(setq x (make-variable))
```
\[11\]

;;; Constrain i to be the intersection of x and (a a b c c c) > (setq i (intersectionv x '(a a b c c c)))

\[12\]

;;; Bind x

```lisp
(make-equal x '(a b d e f))
```
(A B D E F)

;;; Check that the intersection is now also bound > i

(B A)

;;; Create a variable x

```lisp
(setq x (make-variable))
```
\[107\]

;;; Constrain i to be the intersection of (a b c) and x > (setq i (intersectionv '(a b c) x))

\[108\]

;;; Bind i

```lisp
(make-equal i '(a b))
```
(A B)

```lisp
(make-equal x '(c b a) 'failed)
```
Warning: (make-equal X '(C B A)) failed

X = \[107\]; '(C B A) = (C B A) FAILED

8\. **intersectionv**uses the Common LISP function **intersection**, which leaves the duplication is-sue open to the LISP implementation.

_21_

_Constraint Handling in Common LISP_

**unionv**

_The Extension to SCREAMER_

\[Function\]

**Synopsis:** (**unionv** _x y_)

**Description:** The function **unionv**returns a variable, _z_, constrained to be the union of the two sets _x_ and _y_. The output variable _z_ becomes bound as soon as both _x_ and _y_ be-

come bound. If either _x_ or _y_ has duplicate entries, the redundant entries may or may not

appear in the result9. Otherwise, if _z_ becomes bound before _x_ or _y_, then the elements of the latter are constrained to be members of _z_.

;;; Create a constraint variable > (setq x (make-variable))

\[145\]

;;; Constrain u to be the union of x and (a a b c c c) > (setq u (unionv x '(a a b c c c)))

\[146\]

;;; Bind x

```lisp
(make-equal x '(a b d e f))
```
(A B D E F)

;;; Check the binding of u > u

(F E D A A B C C C)

;;; Create a constraint variable > (setq x (make-variable))

\[149\]

;;; Constrain u to tbe the union of x and (a b c) > (setq u (unionv x '(a b c)))

\[150\]

;;; Bind u first to be (a b c d) > (make-equal u '(a b c d))

(A B C D)

```lisp
(make-equal x '(d e f) 'failed)
```
Warning: (make-equal X '(D E F)) failed

X = \[149\]; '(D E F) = (D E F) FAILED

9\. **unionv**uses the Common LISP function **union**, which leaves the duplication issue open to the LISP implementation.

_22_

_Constraint Handling in Common LISP_

**bag-equalv**

_The Extension to SCREAMER_

\[Function\]

**Synopsis:** (**bag-equalv** _x y_)

**Description:** The function **bag-equalv**returns a boolean variable, _z_, which is con-strained to indicate whether its two list arguments are equal when interpreted as bags. In other words, as soon as both _x_ and _y_ become bound, _z_ becomes bound to true if _x_ and _y_ not only have the same members, but also the same number of each of those mem-bers; otherwise _z_ becomes bound to false.

**Examples**

```lisp
(setq x '(a a b c c c))
```
(A A B C C C)

```lisp
(setq y (make-variable))
```
\[300\]

```lisp
(setq same-bag (bag-equalv x y))
```
\[301 Boolean\]

```lisp
(make-equal y '(c a c b c a))
```
(C A C B C A)

```lisp
same-bag
```
T

;;; In this example, the second list has an extra c ;;; So the lists are set-equalv, but not bag-equalv > (bag-equalv '(a b c) '(a b c c))

NIL

**subsetpv** \[Function\]

**Synopsis:** (**subsetpv** _x y_)

**Description:** The function **subsetpv**returns a boolean variable which is constrained to indicate whether _x_ Í _y_ if the two lists _x_ and _y_ are interpreted as sets. Note that the related function **proper-subsetv**can easily be defined as (**andv** (**subsetpv**_x y_) (**not** (**set-equalv**_x y_))).

;;; State the necessary ingredients for some recipe > (setq ingredients '(eggs flour milk salt sugar)) (EGGS FLOUR MILK SALT SUGAR)

;;; We don’t know what ingredients are avaialble yet > (setq available (make-variable))

\[366\]

;;; It is only possible to prepare the recipe if the necessary ;;; ingredients is a subset of the available ingredients

\> (setq can-prepare (subsetpv ingredients available)) \[378 Boolean\]

;;; State which ingredients are available

\> (make-equal available '(salt pepper milk juice eggs sugar rai-sins flour))

(SALT PEPPER MILK JUICE EGGS SUGAR RAISINS FLOUR)

_23_

_Constraint Handling in Common LISP_ _The Extension to SCREAMER_

;;; Check that can-prepare has become bound > can-prepare

T

**3.6 Arrays**

**make-arrayv** \[Function\]

**Synopsis:** (**make-arrayv**_d_)

**Description:** The function **make-arrayv**returns a variable which is constrained to be an array with dimensions given by_d_. More precisely, the function actually generates an array with dimensions _d_ in which every element is automatically assigned to be a new, unique, unbound constraint variable. Note that since a constraint variable is generated for every place in the array, there is a memory overhead in generating large ar-rays using this function. The argument _d_ can be either a list of integers or a constraint variable which later becomes bound to a list of integers.

**Examples**

```lisp
(d
```

