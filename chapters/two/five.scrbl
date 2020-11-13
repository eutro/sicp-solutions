#lang scribble/manual
@(require racket/sandbox
          (for-syntax racket/base)
          scribble/example
          scribble-math/dollar
          "../sicp-eval.rkt")

@title[#:style (with-html5 manual-doc-style)]{Systems with Generic Operations}
@(use-katex)

@section{Exercise 2.77}

The evaluation looks like this:

@racketblock[(magnitude z)
             (apply-generic 'magnitude z) (code:comment @#,code{dispatches on '(complex)})
             (magnitude (contents z))
             (magnitude (cdr z))
             (apply-generic 'magnitude (cdr z)) (code:comment @#,code{dispatches on '(rectangular)})
             (sqrt (+ (square (real-part (cddr z)))
                      (square (imag-part (cddr z))))) (code:comment @#,code{from rectangular package})
             (sqrt (+ (square (caddr z))
                      (square (cdddr z))))
             (sqrt (+ 9
                      16))
             (sqrt 25)
             5]

@section{Exercise 2.78}

@sicpnl[(define (attach-tag type-tag contents)
          (cond [(eqv? type-tag 'scheme-number) contents]
                [else  (cons type-tag contents)]))
        (define (type-tag datum)
          (cond [(number? datum) 'scheme-number]
                [(pair? datum) (car datum)]
                [else (error "Bad tagged datum -- TYPE-TAG" datum)]))
        (define (contents datum)
          (cond [(number? datum) datum]
                [(pair? datum) (cdr datum)]
                [else (error "Bad tagged datum -- CONTENTS" datum)]))]

@sicp[#:label "Copied:"
      (define (apply-generic op . args)
        (let ((type-tags (map type-tag args)))
          (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (error
                 "No method for these types -- APPLY-GENERIC"
                 (list op type-tags))))))
      (define (install-scheme-number-package)
        (define (tag x)
          (attach-tag 'scheme-number x))
        (put 'add '(scheme-number scheme-number)
             (lambda (x y) (tag (+ x y))))
        (put 'sub '(scheme-number scheme-number)
             (lambda (x y) (tag (- x y))))
        (put 'mul '(scheme-number scheme-number)
             (lambda (x y) (tag (* x y))))
        (put 'div '(scheme-number scheme-number)
             (lambda (x y) (tag (/ x y))))
        (put 'make 'scheme-number
             (lambda (x) (tag x)))
        'done)
      (define (add x y) (apply-generic 'add x y))
      (define (sub x y) (apply-generic 'sub x y))
      (define (mul x y) (apply-generic 'mul x y))
      (define (div x y) (apply-generic 'div x y))]

@sicp[(install-scheme-number-package)
      (add 1 2)
      (sub 2 1)
      (mul 2 3)
      (div 6 3)]

@section{Exercise 2.79}

@sicp[#:label "The top-level definition:"
      (define (equ? x y)
        (apply-generic 'equ? x y))]

@#reader scribble/comment-reader
(sicp #:label "Lots of copied code here..."
      (define (install-rational-package)
        ;; internal procedures
        (define (numer x) (car x))
        (define (denom x) (cdr x))
        (define (make-rat n d)
          (let ((g (gcd n d)))
            (cons (/ n g) (/ d g))))
        (define (add-rat x y)
          (make-rat (+ (* (numer x) (denom y))
                       (* (numer y) (denom x)))
                    (* (denom x) (denom y))))
        (define (sub-rat x y)
          (make-rat (- (* (numer x) (denom y))
                       (* (numer y) (denom x)))
                    (* (denom x) (denom y))))
        (define (mul-rat x y)
          (make-rat (* (numer x) (numer y))
                    (* (denom x) (denom y))))
        (define (div-rat x y)
          (make-rat (* (numer x) (denom y))
                    (* (denom x) (numer y))))

        ;; interface to rest of the system
        (define (tag x) (attach-tag 'rational x))
        (put 'add '(rational rational)
             (lambda (x y) (tag (add-rat x y))))
        (put 'sub '(rational rational)
             (lambda (x y) (tag (sub-rat x y))))
        (put 'mul '(rational rational)
             (lambda (x y) (tag (mul-rat x y))))
        (put 'div '(rational rational)
             (lambda (x y) (tag (div-rat x y))))

        (put 'make 'rational
             (lambda (n d) (tag (make-rat n d))))

        ;; ~~~~~~~~~~~~~~~~~~ EQU? ADDITION ~~~~~~~~~~~~~~~~~~
        (define (equ? x y)
          (and (= (numer x) (numer y))
               (= (denom x) (denom y))))
        (put 'equ? '(rational rational) equ?)
        ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        'done)
      (define (make-rational n d)
        ((get 'make 'rational) n d))

      (define (install-rectangular-package)
        ;; internal procedures
        (define (real-part z) (car z))
        (define (imag-part z) (cdr z))
        (define (make-from-real-imag x y) (cons x y))
        (define (magnitude z)
          (sqrt (+ (square (real-part z))
                   (square (imag-part z)))))
        (define (angle z)
          (atan (imag-part z) (real-part z)))
        (define (make-from-mag-ang r a)
          (cons (* r (cos a)) (* r (sin a))))
        ;; interface to the rest of the system
        (define (tag x) (attach-tag 'rectangular x))
        (put 'real-part '(rectangular) real-part)
        (put 'imag-part '(rectangular) imag-part)
        (put 'magnitude '(rectangular) magnitude)
        (put 'angle '(rectangular) angle)
        (put 'make-from-real-imag 'rectangular
             (lambda (x y) (tag (make-from-real-imag x y))))
        (put 'make-from-mag-ang 'rectangular
             (lambda (r a) (tag (make-from-mag-ang r a))))
        'done)

      (define (install-polar-package)
        ;; internal procedures
        (define (magnitude z) (car z))
        (define (angle z) (cdr z))
        (define (make-from-mag-ang r a) (cons r a))
        (define (real-part z)
          (* (magnitude z) (cos (angle z))))
        (define (imag-part z)
          (* (magnitude z) (sin (angle z))))
        (define (make-from-real-imag x y)
          (cons (sqrt (+ (square x) (square y)))
                (atan y x)))
        ;; interface to the rest of the system
        (define (tag x) (attach-tag 'polar x))
        (put 'real-part '(polar) real-part)
        (put 'imag-part '(polar) imag-part)
        (put 'magnitude '(polar) magnitude)
        (put 'angle '(polar) angle)
        (put 'make-from-real-imag 'polar
             (lambda (x y) (tag (make-from-real-imag x y))))
        (put 'make-from-mag-ang 'polar
             (lambda (r a) (tag (make-from-mag-ang r a))))
        'done)

      (define (install-complex-package)
        ;; imported procedures from rectangular and polar packages
        (define (make-from-real-imag x y)
          ((get 'make-from-real-imag 'rectangular) x y))
        (define (make-from-mag-ang r a)
          ((get 'make-from-mag-ang 'polar) r a))
        ;; ~~~~~~~~~~~~~~~~~~ EQU? ADDITION ~~~~~~~~~~~~~~~~~~
        (define (real-part z)
          (apply-generic 'real-part z))
        (define (imag-part z)
          (apply-generic 'imag-part z))
        ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ;; internal procedures
        (define (add-complex z1 z2)
          (make-from-real-imag (+ (real-part z1) (real-part z2))
                               (+ (imag-part z1) (imag-part z2))))
        (define (sub-complex z1 z2)
          (make-from-real-imag (- (real-part z1) (real-part z2))
                               (- (imag-part z1) (imag-part z2))))
        (define (mul-complex z1 z2)
          (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                             (+ (angle z1) (angle z2))))
        (define (div-complex z1 z2)
          (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                             (- (angle z1) (angle z2))))
        ;; interface to rest of the system
        (define (tag z) (attach-tag 'complex z))
        (put 'add '(complex complex)
             (lambda (z1 z2) (tag (add-complex z1 z2))))
        (put 'sub '(complex complex)
             (lambda (z1 z2) (tag (sub-complex z1 z2))))
        (put 'mul '(complex complex)
             (lambda (z1 z2) (tag (mul-complex z1 z2))))
        (put 'div '(complex complex)
             (lambda (z1 z2) (tag (div-complex z1 z2))))
        (put 'make-from-real-imag 'complex
             (lambda (x y) (tag (make-from-real-imag x y))))
        (put 'make-from-mag-ang 'complex
             (lambda (r a) (tag (make-from-mag-ang r a))))

        ;; ~~~~~~~~~~~~~~~~~~ EQU? ADDITION ~~~~~~~~~~~~~~~~~~
        (define (equ? x y)
          (and (= (real-part x) (real-part y))
               (= (imag-part x) (imag-part y))))
        (put 'equ? '(complex complex) equ?)
        ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        'done)

      (define (make-complex-from-real-imag x y)
        ((get 'make-from-real-imag 'complex) x y))
      (define (make-complex-from-mag-ang r a)
        ((get 'make-from-mag-ang 'complex) r a)))

@sicp[#:label "Finally:"
      (put 'equ? '(scheme-number scheme-number) =)]
(this would go in the scheme-number package)

@sicp[#:label "Install them all..."
      (install-rational-package)
      (install-rectangular-package)
      (install-polar-package)
      (install-complex-package)]

@sicp[(equ? 1 1)
      (let ([rat (make-rational 1 2)])
        (equ? rat rat))
      (equ? (make-complex-from-real-imag 1 0)
            (make-complex-from-mag-ang 1 0))]

@section{Exercise 2.80}

These can just be defined in terms of @tt{equ?}.

@sicpnl[(print-el (make-rational 0 1))
        (print-el (make-complex-from-real-imag 0 0))
        (print-el (make-complex-from-mag-ang 0 0))

        (equ? (make-rational 0 1) (make-rational 0 100))]

@sicpnl[(define (pipe a b)
          (lambda (first . rest)
            (apply b (a first) rest)))]

@sicpnl[(put '=zero? '(rational) (pipe (partial attach-tag 'rational)
                                       (partial equ? (make-rational 0 1))))
        (put '=zero? '(complex) (pipe (partial attach-tag 'complex)
                                      (partial equ? (make-complex-from-real-imag 0 0))))
        (put '=zero? '(scheme-number) (partial equ? 0))]

(These would go into the respective packages)

@sicp[#:label "Then just the top-level definition:"
      (define (=zero? z)
        (apply-generic '=zero? z))]

@sicp[(=zero? (make-rational 0 100))
      (=zero? (make-complex-from-mag-ang 0 0))
      (=zero? 0)]

@section{Exercise 2.81}

@sicp[#:hidden ;; h
      (define coercion-table (make-table))
      (define get-coercion (coercion-table 'lookup-proc))
      (define put-coercion (coercion-table 'insert-proc!))]

@sicp[#:label "Copied and slightly altered for readability:"
      (define (apply-generic op . args)
        (let* ([type-tags (map type-tag args)]
               [proc (get op type-tags)])
          (if proc
              (apply proc (map contents args))
              (if (= (length args) 2)
                  (let* ([type1 (car type-tags)]
                         [type2 (cadr type-tags)]
                         [a1 (car args)]
                         [a2 (cadr args)]
                         [t1->t2 (get-coercion type1 type2)]
                         [t2->t1 (get-coercion type2 type1)])
                    (cond [t1->t2 (apply-generic op (t1->t2 a1) a2)]
                          [t2->t1 (apply-generic op a1 (t2->t1 a2))]
                          [else (error "No method for these types"
                                       (list op type-tags))]))
                  (error "No method for these types"
                         (list op type-tags))))))

      (define (scheme-number->complex n)
        (make-complex-from-real-imag (contents n) 0))
      (put-coercion 'scheme-number 'complex scheme-number->complex)]

@sicp[(equ? (make-complex-from-real-imag 1 1)
            (add 1
                 (make-complex-from-real-imag 0 1)))]

@subsection{Exercise 2.81.a}

@#reader scribble/comment-reader
(sicp
 #:label "Even more copying:"
 (define (scheme-number->scheme-number n) n)
 (define (complex->complex z) z)
 (put-coercion 'scheme-number 'scheme-number
               scheme-number->scheme-number)
 (put-coercion 'complex 'complex complex->complex)
 (define (exp x y) (apply-generic 'exp x y))

 ;; following added to Scheme-number package
 (put 'exp '(scheme-number scheme-number)
      ;; using primitive expt
      (lambda (x y) (tag (expt x y)))))

It coerces the first object to its own type, then recurses indefinitely.

The procedure @italic{is} tail-recursive, so it won't overflow the call
stack, merely hang.

@sicp[(exp (make-complex-from-real-imag 1 1)
           (make-complex-from-real-imag 1 1))]

@subsection{Exercise 2.81.b}

Louis is incorrect.

Attempting an @italic{implemented} generic operation on
two types that can't be coerced to themselves works fine:

@sicp[(print-el
       (add (make-rational 1 2)
            (make-rational 1 2)))]

And if it's unimplemented, it will also error properly,
(but only after attempting to coerce the objects
to their own types):

@sicp[(print-el
       (exp (make-rational 1 2)
            (make-rational 1 2)))]

@subsection{Exercise 2.81.c}

@sicpnl[(define (apply-generic op . args)
          (let* ([type-tags (map type-tag args)]
                 [proc (get op type-tags)])
            (if proc
                (apply proc (map contents args))
                (if (and (= (length args) 2)
                         (not (eq? (car type-tags)
                                   (cadr type-tags))))
                    (let* ([type1 (car type-tags)]
                           [type2 (cadr type-tags)]
                           [a1 (car args)]
                           [a2 (cadr args)]
                           [t1->t2 (get-coercion type1 type2)]
                           [t2->t1 (get-coercion type2 type1)])
                      (cond [t1->t2 (apply-generic op (t1->t2 a1) a2)]
                            [t2->t1 (apply-generic op a1 (t2->t1 a2))]
                            [else (error "No method for these types"
                                         (list op type-tags))]))
                    (error "No method for these types"
                           (list op type-tags))))))]

Which has the result that this no longer recurses indefinitely,
since coercions aren't looked up if the types are the same:

@sicp[(exp (make-complex-from-real-imag 1 1)
           (make-complex-from-real-imag 1 1))]

@section{Exercise 2.82}

@sicpnl[(define (all? coll)
          (fold-left (lambda (a b) (and a b))
                     true
                     coll))

        (define (all-same? coll)
          (or (null? coll)
              (all? (map (partial eq? (car coll)) coll))))]

@sicp[(all-same? '(a a a a))
      (all-same? '(a a b a))]

@sicpnl[(define (list-repeated n x)
          (define (loop coll n)
            (if (= 0 n)
                coll
                (loop (cons x coll)
                      (dec n))))
          (loop '() n))

        (define (coerce-all type coll)
          (if (null? coll)
              '()
              (let* ([f (car coll)]
                     [ft (type-tag f)]
                     [ft->type (if (eq? ft type)
                                   (lambda (x) x)
                                   (get-coercion ft type))])
                (and ft->type
                     (let ([next (coerce-all type (cdr coll))])
                       (and next
                            (cons (ft->type f) next)))))))

        (define (apply-generic op . args)
          (let* ([type-tags (map type-tag args)]
                 [proc (get op type-tags)])
            (if proc
                (apply proc (map contents args))
                (if (not (all-same? type-tags))
                    (letrec ([arg-count (length args)]
                             [loop (lambda (types)
                                     (if (null? types)
                                         (error "No method for these types"
                                                (list op type-tags))
                                         (let* ([target-type (car types)]
                                                [types-if-coerced (list-repeated arg-count target-type)]
                                                [proc (get op types-if-coerced)])
                                           (if proc
                                               (let ([coerced (coerce-all target-type args)])
                                                 (if coerced
                                                     (apply proc (map contents coerced))
                                                     (loop (cdr types))))
                                               (loop (cdr types))))))])
                      (loop type-tags))
                    (error "No method for these types"
                           (list op type-tags))))))]

@sicp[#:label "An example generic procedure for adding four complex numbers:"
      (let ([tag (lambda (x) (cons 'complex x))])
        (put 'add '(complex complex complex complex)
             (lambda (a b c d)
               (add (add (tag a)
                         (tag b))
                    (add (tag c)
                         (tag d))))))

      (define (add . args)
        (apply apply-generic 'add args))]

@sicp[(print-el
       (add (make-complex-from-real-imag 1 1)
            (make-complex-from-real-imag 1 1)
            (make-complex-from-real-imag 1 1)
            (make-complex-from-real-imag 1 1)))
      (print-el
       (add (make-complex-from-real-imag 1 1)
            1
            (make-complex-from-real-imag 1 1)
            (make-complex-from-real-imag 1 1)))
      (print-el
       (add 1
            1
            (make-complex-from-real-imag 1 1)
            (make-complex-from-real-imag 1 1)))
      (print-el
       (add 1
            (make-complex-from-real-imag 1 1)
            1
            1))]

This would not be sufficient for mixed-type operations, i.e.
operations that operate on different types.

For example, an @tt{exp} definition for the types
@racket[(complex rational)], would not be looked
up when @tt{exp} is called with the types
@racket[(complex scheme-number)], even if
@tt{scheme-number} can be coerced to @tt{rational}.

@section{Exercise 2.83}

@sicpnl[(define (raise obj)
          (apply-generic 'raise obj))]

@sicp[#:label "Some new types, and their generic operations:"
      (define (make-integer n)
        (attach-tag 'integer n))
      (define (make-real x)
        (attach-tag 'real x))

      (put 'raise '(integer)
           (lambda (int)
             (make-rational int 1)))

      (put 'raise '(rational)
           (lambda (rat)
             (make-real (/ (numer rat)
                           (denom rat)))))

      (put 'raise '(real)
           (lambda (real)
             (make-complex-from-real-imag real 0)))]

(These would go in their respective packages)

@sicp[(define a-number (make-integer 1))
      (print-el a-number)
      (define a-number (raise a-number))
      (print-el a-number)
      (define a-number (raise a-number))
      (print-el a-number)
      (define a-number (raise a-number))
      (print-el a-number)
      (define a-number (raise a-number))]

@section{Exercise 2.84}

The upper type can simply be obtained by @tt{raise}-ing
the object and taking its type tag. First, the existence
of the @tt{raise} procedure for the type needs to be checked
to determine if it's the highest type.

@tt{raise} can be redefined as such:
@sicpnl[(define (raise obj)
          (let ([raiser (get 'raise (list (type-tag obj)))])
            (if raiser
                (raiser (contents obj))
                #f)))]

@sicpnl[(define (type>? a b)
          "Checks whether the type of a is higher than the type of b."
          (if (eq? (type-tag a)
                   (type-tag b))
              #f
              (let ([ra (raise a)]
                    [rb (raise b)])
                (cond [(not ra) #t]
                      [(not rb) #f]
                      [else (type>? ra rb)]))))

        (define (max-by gt? first . others)
          (if (null? others)
              first
              (let ([second (car others)])
                (apply max-by
                       gt?
                       (if (gt? first second)
                           first
                           second)
                       (cdr others)))))

        (define (raise-to type obj)
          (if (eq? (type-tag obj)
                   type)
              obj
              (let ([raised (raise obj)])
                (if raised
                    (raise-to type raised)
                    (error "Couldn't raise object -- RAISE-TO" type obj)))))

        (define (apply-generic op . args)
          (let* ([type-tags (map type-tag args)]
                 [proc (get op type-tags)])
            (cond [proc (apply proc (map contents args))]
                  [(all-same? type-tags) (error "No method for these types"
                                                (list op type-tags))]
                  [else
                   (apply apply-generic
                          op
                          (map (partial raise-to
                                        (type-tag (apply max-by type>? args)))
                               args))])))]

@sicp[(print-el (add (make-complex-from-real-imag 1 1)
                     (make-integer 2)))]

Adding a higher type than complex numbers, like quaternions,
is as simple as adding the raising procedure.

@sicp[#:label "A simple definition of quaternions:"
      (define (make-quat r i j k)
        (lambda (f)
          (f r i j k)))
      (define (quat-r quat)
        (quat (lambda (r i j k) r)))
      (define (quat-i quat)
        (quat (lambda (r i j k) i)))
      (define (quat-j quat)
        (quat (lambda (r i j k) j)))
      (define (quat-k quat)
        (quat (lambda (r i j k) k)))
      (define (print-quat quat)
        (quat (lambda (r i j k)
                (display r)
                (display " + ")
                (display i)
                (display "i + ")
                (display j)
                (display "j + ")
                (display k)
                (display "k"))))]

@sicp[#:label "Then adding it to the type system:"
      (define (make-quaternion r i j k)
        (attach-tag 'quaternion
                    (make-quat r i j k)))
      (put 'raise '(complex)
           (let ([real-part (lambda (z) (apply-generic 'real-part z))]
                 [imag-part (lambda (z) (apply-generic 'imag-part z))])
             (lambda (c)
               (make-quaternion (real-part c)
                                (imag-part c)
                                0
                                0))))
      (put 'add '(quaternion quaternion)
           (lambda (a b)
             (make-quaternion (+ (quat-r a)
                                 (quat-r b))
                              (+ (quat-i a)
                                 (quat-i b))
                              (+ (quat-j a)
                                 (quat-j b))
                              (+ (quat-k a)
                                 (quat-k b)))))]

@sicpnl[(print-quat (contents (add (make-quaternion 0 0 3 4)
                                   (add (make-complex-from-real-imag 0 2)
                                        (make-integer 1)))))]

@section{Exercise 2.85}

@sicp[#:label "First, generic displaying:"
      (put 'display '(integer) display)
      (put 'display '(rational)
           (lambda (n)
             (display (numer n))
             (display "/")
             (display (denom n))))
      (put 'display '(real) display)
      (put 'display '(complex) print-complex-cartesian)
      (put 'display '(quaternion) print-quat)
      (define (display-generic obj)
        (apply-generic 'display obj))
      (define (print-generic obj)
        (display-generic obj)
        (newline))]

@sicp[(define a-number (make-integer 1))
      (print-generic a-number)
      (define a-number (raise a-number))
      (print-generic a-number)
      (define a-number (raise a-number))
      (print-generic a-number)
      (define a-number (raise a-number))
      (print-generic a-number)
      (define a-number (raise a-number))
      (print-generic a-number)]

@sicp[#:label "Then generic drop operations:"
      (put 'project '(quaternion)
           (lambda (quat)
             (make-complex-from-real-imag (quat-r quat)
                                          (quat-i quat))))
      (put 'project '(complex)
           (let ([real-part (lambda (z) (apply-generic 'real-part z))])
             (lambda (c)
               (make-real (real-part c)))))
      (put 'project '(real)
           (lambda (r)
             (let ([rat (inexact->exact (rationalize r 1/100))])
               (make-rational (numerator rat)
                              (denominator rat)))))
      (put 'project '(rational)
           (lambda (rat)
             (make-integer (quotient (numer rat)
                                     (denom rat)))))]

@sicp[(define a-number (make-quaternion pi 4 5 6))
      (print-generic a-number)
      (define a-number (apply-generic 'project a-number))
      (print-generic a-number)
      (define a-number (apply-generic 'project a-number))
      (print-generic a-number)
      (define a-number (apply-generic 'project a-number))
      (print-generic a-number)
      (define a-number (apply-generic 'project a-number))
      (print-generic a-number)]

@sicp[#:label "Finally, definitions of equivalence."
      (put 'equ? '(integer integer) =)
      (put 'equ? '(rational rational)
           (lambda (a b)
             (and (= (numer a)
                     (numer b))
                  (= (denom a)
                     (denom b)))))
      (put 'equ? '(real real) =)
      (put 'equ? '(quaternion quaternion)
           (lambda (a b)
             (and (= (quat-r a)
                     (quat-r b))
                  (= (quat-i a)
                     (quat-i b))
                  (= (quat-j a)
                     (quat-j b))
                  (= (quat-k a)
                     (quat-k b)))))]

@sicp[#:label "Then the top-level drop function:"
      (define (drop obj)
        (let ([dropped (project obj)])
          (cond [(not dropped) obj]
                [(equ? dropped obj) (drop dropped)]
                [else obj])))
      (define (project obj)
        (let ([dropper (get 'project (list (type-tag obj)))])
          (if dropper
              (dropper (contents obj))
              #f)))]

(Note that the @tt{apply-generic} of @tt{equ?} automatically raises the dropped object back up.)

@sicp[(define (type-and-print obj)
        (display (type-tag obj))
        (newline)
        (print-generic obj))
      (type-and-print (drop (make-quaternion 1 2 3 4)))
      (type-and-print (drop (make-quaternion 1 2 0 0)))
      (type-and-print (drop (make-quaternion pi 0 0 0)))
      (type-and-print (drop (make-quaternion 1/2 0 0 0)))
      (type-and-print (drop (make-quaternion 1 0 0 0)))]

@sicp[#:label "Then, apply-generic:"
      (define (apply-generic op . args)
        (let* ([type-tags (map type-tag args)]
               [proc (get op type-tags)])
          (cond [proc (let ([ret (apply proc (map contents args))])
                        (if (pair? ret)
                            (drop ret)
                            ret))]
                [(all-same? type-tags) (error "No method for these types"
                                              (list op type-tags))]
                [else
                 (apply apply-generic
                        op
                        (map (partial raise-to
                                      (type-tag (apply max-by
                                                       type>?
                                                       args)))
                             args))])))]

@sicp[(type-and-print (add (make-quaternion 1 2 3 4)
                           (make-quaternion 1 -2 -3 -4)))]

@sicp[#:label "Quaternion multiplication for fun:"
      (put 'mul '(quaternion quaternion)
           (lambda (a b)
             (make-quaternion
              (- (* (quat-r a)
                    (quat-r b))

                 (+ (* (quat-i a)
                       (quat-i b))
                    (* (quat-j a)
                       (quat-j b))
                    (* (quat-k a)
                       (quat-k b))))

              (+ (+ (* (quat-r a)
                       (quat-i b))
                    (* (quat-i a)
                       (quat-r b)))

                 (- (* (quat-j a)
                       (quat-k b))
                    (* (quat-k a)
                       (quat-j b))))

              (+ (+ (* (quat-r a)
                       (quat-j b))
                    (* (quat-j a)
                       (quat-r b)))

                 (- (* (quat-k a)
                       (quat-i b))
                    (* (quat-i a)
                       (quat-k b))))

              (+ (+ (* (quat-r a)
                       (quat-k b))
                    (* (quat-k a)
                       (quat-r b)))

                 (- (* (quat-i a)
                       (quat-j b))
                    (* (quat-j a)
                       (quat-i b)))))))]

@sicp[#:label "For convenience:"
      (define (mul* first . rest)
        (if (null? rest)
            first
            (apply mul*
                   (mul first (car rest))
                   (cdr rest))))]

@sicp[(type-and-print (mul* (make-quaternion 1 0 0 0)
                            (make-quaternion 0 1 0 0)
                            (make-quaternion 0 0 1 0)
                            (make-quaternion 0 0 0 1)))
      (type-and-print (mul (make-quaternion 1 2 3 4)
                           (make-integer 2)))
      (type-and-print (mul (make-quaternion 0 0 1 0)
                           (make-quaternion 0 0 0 1)))]

@section{Exercise 2.86}

The complex packages just need to use the generic operations instead
of operations solely on scheme numbers.

Also, the other number packages need to implement operations like
sine, cosine, arctangent and square-root.

@sicp[#:label "Complex packages:"
      (define (install-rectangular-package)
        ;; internal procedures
        (define (real-part z) (car z))
        (define (imag-part z) (cdr z))
        (define (make-from-real-imag x y) (cons x y))
        (define (magnitude z)
          (square-root (add (mul (real-part z) (real-part z))
                            (mul (imag-part z) (imag-part z)))))
        (define (angle z)
          (arctangent (imag-part z) (real-part z)))
        (define (make-from-mag-ang r a)
          (cons (mul r (cosine a)) (mul r (sine a))))
        ;; interface to the rest of the system
        (define (tag x) (attach-tag 'rectangular x))
        (put 'real-part '(rectangular) real-part)
        (put 'imag-part '(rectangular) imag-part)
        (put 'magnitude '(rectangular) magnitude)
        (put 'angle '(rectangular) angle)
        (put 'make-from-real-imag 'rectangular
             (lambda (x y) (tag (make-from-real-imag x y))))
        (put 'make-from-mag-ang 'rectangular
             (lambda (r a) (tag (make-from-mag-ang r a))))
        'done)

      (define (install-polar-package)
        ;; internal procedures
        (define (magnitude z) (car z))
        (define (angle z) (cdr z))
        (define (make-from-mag-ang r a) (cons r a))
        (define (real-part z)
          (mul (magnitude z) (cosine (angle z))))
        (define (imag-part z)
          (mul (magnitude z) (sine (angle z))))
        (define (make-from-real-imag x y)
          (cons (square-root (add (square x) (square y)))
                (arctangent y x)))
        ;; interface to the rest of the system
        (define (tag x) (attach-tag 'polar x))
        (put 'real-part '(polar) real-part)
        (put 'imag-part '(polar) imag-part)
        (put 'magnitude '(polar) magnitude)
        (put 'angle '(polar) angle)
        (put 'make-from-real-imag 'polar
             (lambda (x y) (tag (make-from-real-imag x y))))
        (put 'make-from-mag-ang 'polar
             (lambda (r a) (tag (make-from-mag-ang r a))))
        'done)

      (define (install-complex-package)
        ;; imported procedures from rectangular and polar packages
        (define (make-from-real-imag x y)
          ((get 'make-from-real-imag 'rectangular) x y))
        (define (make-from-mag-ang r a)
          ((get 'make-from-mag-ang 'polar) r a))
        (define (real-part z)
          (apply-generic 'real-part z))
        (define (imag-part z)
          (apply-generic 'imag-part z))
        (define (add-complex z1 z2)
          (make-from-real-imag (add (real-part z1) (real-part z2))
                               (add (imag-part z1) (imag-part z2))))
        (define (sub-complex z1 z2)
          (make-from-real-imag (add (real-part z1) (real-part z2))
                               (add (imag-part z1) (imag-part z2))))
        (define (mul-complex z1 z2)
          (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                             (add (angle z1) (angle z2))))
        (define (div-complex z1 z2)
          (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                             (sub (angle z1) (angle z2))))
        ;; interface to rest of the system
        (define (tag z) (attach-tag 'complex z))
        (put 'add '(complex complex)
             (lambda (z1 z2) (tag (add-complex z1 z2))))
        (put 'sub '(complex complex)
             (lambda (z1 z2) (tag (sub-complex z1 z2))))
        (put 'mul '(complex complex)
             (lambda (z1 z2) (tag (mul-complex z1 z2))))
        (put 'div '(complex complex)
             (lambda (z1 z2) (tag (div-complex z1 z2))))
        (put 'make-from-real-imag 'complex
             (lambda (x y) (tag (make-from-real-imag x y))))
        (put 'make-from-mag-ang 'complex
             (lambda (r a) (tag (make-from-mag-ang r a))))
        (put 'equ? '(complex complex)
             (lambda (x y)
               (and (equ? (real-part x) (real-part y))
                    (equ? (imag-part x) (imag-part y)))))
        'done)]

@sicp[#:label "Install again:"
      (install-rectangular-package)
      (install-polar-package)
      (install-complex-package)]

@sicp[#:label "Some of the required generic operations:"
      (put 'add '(rational rational)
           (lambda (a b)
             (make-rational (+ (* (numer a)
                                  (denom b))
                               (* (numer b)
                                  (denom a)))
                            (* (denom a)
                               (denom b)))))
      (put 'cosine '(real)
           (lambda (r)
             (make-real (cos r))))
      (put 'mul '(real real)
           (lambda (a b)
             (make-real (* a b))))
      (put 'add '(real real)
           (lambda (a b)
             (make-real (+ a b))))
      (put 'sine '(real)
           (lambda (r)
             (make-real (sin r))))
      (define (cosine x)
        (apply-generic 'cosine x))
      (define (sine x)
        (apply-generic 'sine x))]

@sicp[#:label "The raising and projecting functions also needs to be redefined:"
      (put 'raise '(real)
           (lambda (real)
             (make-complex-from-real-imag (make-real real) (make-real 0))))
      (put 'project '(complex)
           (lambda (z) (apply-generic 'real-part z)))]

@sicp[#:label "Also, Quaternions will be removed from the tower:"
      (put 'raise '(complex) #f)]

@sicp[#:label "For convenience:"
      (define (add* first . rest)
        (if (null? rest)
            first
            (apply add*
                   (add first (car rest))
                   (cdr rest))))]

@sicp[(print-el (add* (make-complex-from-mag-ang (make-real (sqrt 2))
                                                 (make-real (/ pi 4)))
                      (make-complex-from-real-imag (make-integer 0)
                                                   (make-rational 1 2))
                      (make-complex-from-real-imag (make-real -0.0000000000011278)
                                                   (make-real -0.0000000000011275))))]

@section{Exercise 2.87}

@tt{=zero?} also needs to be implemented for some of the tower types:

@sicpnl[(put '=zero? '(integer)
             (lambda (i)
               (= i 0)))
        (put '=zero? '(rational)
             (lambda (r)
               (= 0 (numer r))))
        (put '=zero? '(real)
             (lambda (r)
               (= r 0)))]

@sicp[(=zero? (make-integer 0))
      (=zero? (make-rational 0 2))
      (=zero? (make-real 0))]

@(define-syntax append-to-poly
   (let ([poly-appended '()]
         [max-calls 4]
         [called 0])
     (lambda (stx)
       ;; for some reason this macro gets
       ;; expanded twice, this is an elegant solution
       (if (= called max-calls)
           (begin (set! called 0)
                  (set! poly-appended '()))
           (set! called (+ called 1)))
       (set! poly-appended
             (append poly-appended
                     (cdr (syntax->list stx))))
       (let ([poly-appended poly-appended])
         #`(sicpnl
            (eval:alts
             #,(let ([f (lambda (datum offset col)
                          (datum->syntax
                           stx
                           datum
                           (list (syntax-source stx)
                                 (+ (syntax-line stx) offset)
                                 col
                                 #f
                                 #f)))])
                 (f (append (list (f 'define -1 1)
                                  (f '(install-polynomial-package) -1 3)
                                  (f '(code:comment "<the rest of the polynomial package ...>") 0 2))
                            (cdr (syntax->list stx)))
                    -1 0))
             (define (install-polynomial-package)
               ;; internal procedures
               ;; representation of poly
               (define (make-poly variable term-list)
                 (cons variable term-list))
               (define (variable p) (car p))
               (define (term-list p) (cdr p))
               (define (same-variable? v1 v2)
                 (and (variable? v1) (variable? v2) (eq? v1 v2)))
               (define (variable? x) (symbol? x))
               ;; representation of terms and term lists
               (define (adjoin-term term term-list)
                 (if (=zero? (coeff term))
                     term-list
                     (cons term term-list)))
               (define (the-empty-termlist) '())
               (define (first-term term-list) (car term-list))
               (define (rest-terms term-list) (cdr term-list))
               (define (empty-termlist? term-list) (null? term-list))
               (define (make-term order coeff) (list order coeff))
               (define (order term) (car term))
               (define (coeff term) (cadr term))

               (define (add-poly p1 p2)
                 (if (same-variable? (variable p1) (variable p2))
                     (make-poly (variable p1)
                                (add-terms (term-list p1)
                                           (term-list p2)))
                     (error "Polys not in same var -- ADD-POLY"
                            (list p1 p2))))
               (define (add-terms L1 L2)
                 (cond ((empty-termlist? L1) L2)
                       ((empty-termlist? L2) L1)
                       (else
                        (let ((t1 (first-term L1)) (t2 (first-term L2)))
                          (cond ((> (order t1) (order t2))
                                 (adjoin-term
                                  t1 (add-terms (rest-terms L1) L2)))
                                ((< (order t1) (order t2))
                                 (adjoin-term
                                  t2 (add-terms L1 (rest-terms L2))))
                                (else
                                 (adjoin-term
                                  (make-term (order t1)
                                             (add (coeff t1) (coeff t2)))
                                  (add-terms (rest-terms L1)
                                             (rest-terms L2)))))))))
               (define (mul-poly p1 p2)
                 (if (same-variable? (variable p1) (variable p2))
                     (make-poly (variable p1)
                                (mul-terms (term-list p1)
                                           (term-list p2)))
                     (error "Polys not in same var -- MUL-POLY"
                            (list p1 p2))))
               (define (mul-terms L1 L2)
                 (if (empty-termlist? L1)
                     (the-empty-termlist)
                     (add-terms (mul-term-by-all-terms (first-term L1) L2)
                                (mul-terms (rest-terms L1) L2))))
               (define (mul-term-by-all-terms t1 L)
                 (if (empty-termlist? L)
                     (the-empty-termlist)
                     (let ((t2 (first-term L)))
                       (adjoin-term
                        (make-term (+ (order t1) (order t2))
                                   (mul (coeff t1) (coeff t2)))
                        (mul-term-by-all-terms t1 (rest-terms L))))))
               ;; interface to rest of the system
               (define (tag p) (attach-tag 'polynomial p))
               (put 'add '(polynomial polynomial)
                    (lambda (p1 p2) (tag (add-poly p1 p2))))
               (put 'mul '(polynomial polynomial)
                    (lambda (p1 p2) (tag (mul-poly p1 p2))))
               (put 'make 'polynomial
                    (lambda (var terms) (tag (make-poly var terms))))
               #,@poly-appended
               'done))
            (install-polynomial-package))))))

Printing would be nice:
@(append-to-poly
  (define (display-poly p)
    (let ([var (variable p)])
      (define (loop terms)
        (let ([term (first-term terms)])
          (display "(")
          (display-generic (coeff term))
          (display ")")
          (if (> (order term) 0)
              (begin (display var)
                     (if (> (order term) 1)
                         (begin
                           (display "^{")
                           (display (order term))
                           (display "}")))))
          (let ([remaining (rest-terms terms)])
            (if (not (empty-termlist? remaining))
                (begin (display " + ")
                       (loop remaining))))))
      (if (not (empty-termlist? p))
          (loop (reverse (term-list p))))))
  (put 'display '(polynomial) display-poly))

A polynomial is just zero if it has no terms:

@(append-to-poly
  (put '=zero? '(polynomial)
       (lambda (p) (empty-termlist? (term-list p)))))

And there aren't any convenient constructors for term lists:

@(append-to-poly
  (define (make-poly* variable . terms)
    (define (loop terms coeff order . remaining)
      (let ([built (adjoin-term (make-term order coeff) terms)])
        (if (null? remaining)
            built
            (apply loop built remaining))))
    (make-poly variable
               (if (not (null? terms))
                   (apply loop (the-empty-termlist) terms)
                   (the-empty-termlist))))
  (put 'make* 'polynomial
       (lambda args (tag (apply make-poly* args)))))

@sicp[(define make-polynomial* (get 'make* 'polynomial))
      (print-generic (make-polynomial*
                      'x
                      (make-polynomial*
                       'y
                       (make-integer 2) 3
                       (make-complex-from-real-imag 1 2) 2) 3
                      (make-integer 1) 2
                      (make-rational 2 3) 1
                      (make-real 3.025) 0))]

@section{Exercise 2.88}

@sicpnl[(define (negate obj)
          (apply-generic 'negate obj))
        (put 'negate '(integer)
             (lambda (i) (make-integer (- i))))
        (put 'negate '(rational)
             (lambda (r)
               (make-rational (- (numer r))
                              (denom r))))
        (put 'negate '(real)
             (lambda (r) (make-real (- r))))
        (put 'negate '(complex)
             (lambda (c) (negate c)))
        (put 'negate '(rectangular)
             (lambda (c)
               (make-complex-from-real-imag (negate (complex-real-part c))
                                            (negate (complex-imag-part c)))))
        (put 'negate '(polar)
             (lambda (c)
               (make-complex-from-mag-ang (complex-magnitude c)
                                          (+ (complex-angle c) pi))))]

@sicp[#:label "Addition has to be defined for these types..."
      (put 'add '(integer integer)
           (lambda (a b) (make-integer (+ a b))))
      (put 'add '(real real)
           (lambda (a b) (make-real (+ a b))))]

@(append-to-poly
  (define (negate-term term)
    (make-term (order term)
               (negate (coeff term))))
  (define (negate-poly poly)
    (make-poly (variable poly)
               (map negate-term (term-list poly))))
  (define (sub-poly a b)
    (add-poly a (negate-poly b)))
  (put 'sub '(polynomial polynomial)
       (lambda (a b) (tag (sub-poly a b))))
  (put 'negate '(polynomial)
       (lambda (p) (tag (negate-poly p)))))

@sicp[(print-generic (sub (make-polynomial*
                           'x
                           (make-polynomial*
                            'y
                            (make-integer 2) 2
                            (make-rational 1 2) 1) 2
                           (make-integer 1) 1
                           (make-rational 2 3) 0)
                          (make-polynomial*
                           'x
                           (make-polynomial*
                            'y
                            (make-real 0.25) 2
                            (make-integer 1) 1) 2
                           (make-integer 3) 1
                           (make-real 1.5) 0)))]

@section{Exercise 2.89}

@sicpnl[;; I needed this
        (define (comp1 f g)
          (lambda args (f (apply g args))))
        (define (comp f . gs)
          (fold-left comp1 f gs))]

To work with a generic termlist representation,
it's important to know what is required to interface
with the representation. These are what I have chosen:

@itemlist[@item{A way to create an empty termlist.}
          @item{A way to adjoin a term to the termlist.}
          @item{A way to convert to a list of terms.}]

The question of what a "term" should be is worth considering. There may
be different ways to represent a term too, so I'll make a package for it also.

@#reader scribble/comment-reader
(sicp
 #:label "The term package:"
 (define (install-term-package)
   (define (make-term order coeff) (list order coeff))
   (define (order term) (car term))
   (define (coeff term) (cadr term))
   ;; interface to rest of system
   (define (tag term) (attach-tag 'term term))
   (put 'make 'term (comp tag make-term))
   (put 'order '(term) order)
   (put 'coeff '(term) coeff)
   'done)
 (install-term-package)
 (define make-term (get 'make 'term))
 (define (order term) (apply-generic 'order term))
 (define (coeff term) (apply-generic 'coeff term)))

@#reader scribble/comment-reader
(sicp
 #:label "The dense termlist package:"
 (define (install-dense-termlist-package)
   (define (the-empty-termlist) '())
   (define (empty-termlist? term-list) (null? term-list))
   (define (adjoin-term term termlist)
     (if (< (order term) 0) (error "Order of term is negative -- ADJOIN-TERM"))
     (if (= (order term) 0)
         (cons (let ([c (coeff term)])
                 (if (=zero? c) '() c))
               termlist)
         (let ([termlist (if (empty-termlist? termlist) '(()) termlist)])
           (cons (car termlist)
                 (adjoin-term (make-term (dec (order term))
                                         (coeff term))
                              (cdr termlist))))))
   (define (termlist->list termlist)
     (let loop ([built '()]
                [termlist termlist]
                [offset 0])
       (if (null? termlist)
           built
           (loop (if (null? (car termlist))
                     built
                     (cons (make-term offset (car termlist))
                           built))
                 (cdr termlist)
                 (inc offset)))))
   ;; interface to rest of system
   (define (tag termlist) (attach-tag 'dense-termlist termlist))
   (put 'adjoin '(term dense-termlist)
        (lambda (term termlist)
          (tag (adjoin-term (attach-tag 'term term) termlist))))
   (put 'empty 'dense-termlist (comp tag the-empty-termlist))
   (put '->list 'dense-termlist termlist->list)
   'done)
 (install-dense-termlist-package))

@sicpnl[(define empty-termlist (get 'empty 'dense-termlist))
        (define (adjoin term termlist)
          (apply-generic 'adjoin term termlist))
        (define (termlist* . terms)
          (fold-right adjoin (empty-termlist) terms))
        (define (termlist->list termlist)
          ((get '->list (type-tag termlist))
           (contents termlist)))]

@sicp[(begin
        (for-each
         (lambda (term)
           (display-generic (coeff term))
           (display "x^")
           (display (order term))
           (display " + "))
         (termlist->list
          (termlist*
           (make-term 100 (make-integer 20))
           (make-term 10 (make-integer 5))
           (make-term 2 (make-integer 2)))))
        (display "0")
        (newline))]
