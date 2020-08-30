#lang scribble/manual
@(require racket/sandbox
          scribble/example
          scribble-math/dollar
          "../sicp-eval.rkt")

@title[#:style (with-html5 manual-doc-style)]{Formulating Abstractions with Higher-Order Procedures}
@(use-katex)

@section{Exercise 1.30}

@sicpnl[
 (define (sum term a next b)
   (define (iter a result)
     (if (> a b)
         result
         (iter (next a) (+ result (term a)))))
   (iter a 0))]

@sicp[
 (define (identity x) x)
 (define (sum-integers a b)
   (sum identity a inc b))
 (sum-integers 1 10)

 (define (sum-cubes a b)
   (sum cube a inc b))
 (sum-cubes 1 10)]

@section{Exercise 1.31}

@subsection{Exercise 1.31.a}

Guess I'll subtly skip ahead for a bit...

@sicp[#:label "Recursive:"
      (define (accumulator-recur op id)
        (define (func term a next b)
          (if (> a b)
              id
              (op (term a)
                  (func term
                        (next a)
                        next
                        b))))
        func)

      (define product (accumulator-recur * 1))]

@sicp[#:label "Factorial:"
      (define (factorial x)
        (product identity 1 inc x))

      (factorial 6)]

@sicp[#:label "Pi:"
      (define (approx-pi n)
        (* 4.0
           (product (lambda (x)
                      (/ (* (dec x)
                            (inc x))
                         (square x)))
                    3
                    (lambda (x)
                      (+ x 2))
                    (* 3 n))))

      (approx-pi 10000)]

@subsection{Exercise 1.31.b}

@sicp[#:label "Iterative:"
      (define (accumulator-iter op id)
        (define (func term a next b)
          (define (iter a result)
            (if (> a b)
                result
                (iter (next a)
                      (op result
                          (term a)))))
          (iter a id))
        func)

      (define product (accumulator-iter * 1))

      (factorial 6)]

@section{Exercise 1.32}

@subsection{Exercise 1.32.a}

This just involves un-currying the previous implementations.

@sicpnl[
 (define (accumulate combiner null-value term a next b)
   ((accumulator-recur combiner null-value) term a next b))

 (define (sum term a next b)
   (accumulate + 0 term a next b))

 (define (product term a next b)
   (accumulate * 1 term a next b))]

@sicp[
 (sum-integers 1 10)
 (factorial 6)]

@subsection{Exercise 1.32.b}

@sicpnl[
 (define (accumulate combiner null-value term a next b)
   ((accumulator-iter combiner null-value) term a next b))]

@sicp[
 (sum-integers 1 10)
 (factorial 6)]

Currying is cooler though.

@section{Exercise 1.33}

@sicpnl[
 (define (filtered-accumulate predicate
                              combiner
                              null-value
                              term
                              a
                              next
                              b)
   (define (iter a result)
     (cond ((> a b) result)
           ((predicate a) (iter (next a)
                                (combiner result
                                          (term a))))
           (else (iter (next a)
                       result))))
   (iter a null-value))]

@subsection{Exercise 1.33.a}

@sicpnl[
 (define (mr-prime? x) (miller-rabin x 10))

 (define (sum-prime-cubes a b)
   (filtered-accumulate mr-prime?
                        +
                        0
                        cube
                        a
                        inc
                        b))]

@sicp[
 (sum-prime-cubes 2 5)]

@subsection{Exercise 1.33.b}

@sicpnl[
 (define (product-relative-primes n)
   (filtered-accumulate (lambda (x)
                          (= 1 (gcd x n)))
                        *
                        1
                        identity
                        1
                        inc
                        (dec n)))]

@sicp[
 (product-relative-primes 10)]

@section{Exercise 1.34}

@racket[(f f)] would evaluate as such,

@racketblock[
 (f f)
 (f 2)
 (2 2)]

and fail, as @tt{2} is not a procedure.

@section{Exercise 1.35}

@${\phi} is defined as

@$${{1 + \phi \over \phi} = \phi}

which can simply be rearranged to

@$${\phi = 1 + {1 \over \phi}}

Thus, it is the fixed point of @${x \rightarrow 1 + {1 \over x}}.

Firstly, define @tt{fixed-point}  as in the book:
@sicpnl[
 (define tolerance 0.00001)
 (define (fixed-point f first-guess)
   (define (close-enough? v1 v2)
     (< (abs (- v1 v2)) tolerance))
   (define (try guess)
     (let ((next (f guess)))
       (if (close-enough? guess next)
           next
           (try next))))
   (try first-guess))]

Then @${\phi} comes to:
@sicpnl[
 (fixed-point (lambda (x) (+ 1
                             (/ 1 x)))
              1.0)]

@section{Exercise 1.36}

@sicpnl[
 (define tolerance 0.00001)
 (define (fixed-point-logged f first-guess)
   (define (close-enough? v1 v2)
     (< (abs (- v1 v2)) tolerance))
   (define (try guess)
     (display guess)
     (newline)
     (let ((next (f guess)))
       (if (close-enough? guess next)
           next
           (try next))))
   (try first-guess))]

@sicp[#:label "Without average damping:"
      (fixed-point-logged (lambda (x)
                            (/ (log 1000)
                               (log x)))
                          5)]

@sicp[#:label "With average damping:"
      (fixed-point-logged (lambda (x)
                            (average x
                                     (/ (log 1000)
                                        (log x))))
                          5)]

@section{Exercise 1.37}

@subsection{Exercise 1.37.a}

@sicp[#:label "Recursive:"
      (define (cont-frac N D k)
        (define (recur i)
          (if (> i k)
              0
              (/ (N i)
                 (+ (D i) (recur (inc i))))))
        (recur 1))]

@sicp[
 (define (approx-phi k)
   (/ 1
      (cont-frac (lambda (i) 1.0)
                 (lambda (i) 1.0)
                 k)))

 (approx-phi 13)]

@${k = 13} results in an estimate correct to four decimal places.

@subsection{Exercise 1.37.b}

@sicp[#:label "Iterative:"
      (define (cont-frac N D k)
        (define (iter i result)
          (if (< i 1)
              result
              (iter (dec i)
                    (/ (N i)
                       (+ (D i) result)))))
        (iter k 0))]

@sicp[
 (approx-phi 13)]

@section{Exercise 1.38}

@sicpnl[
 (define (approx-e k)
   (+ 2
      (cont-frac (lambda (i) 1.0)
                 (lambda (i)
                   (if (= (remainder i 3)
                          2)
                       (* (/ 2 3)
                          (inc i))
                       1))
                 k)))]

@sicp[
 (approx-e 1000)]

@section{Exercise 1.39}

@sicpnl[
 (define (tan-cf x k)
   (cont-frac (let ((-x^2 (- 0 (square x))))
                (lambda (i)
                  (if (= i 1)
                      x
                      -x^2)))
              (lambda (i)
                (dec (* 2 i)))
              k))]

@sicp[
 (let ((pi 3.141593))
   (tan-cf (/ pi 4)
           100))]

@section{Exercise 1.40}

@sicpnl[
 (define (cubic a b c)
   (lambda (x)
     (+ (cube x)
        (* a
           (square x))
        (* b
           x)
        c)))]

@sicp[#:label "Some copying from the book...:"
      (define dx 0.00001)

      (define (deriv g)
        (lambda (x)
          (/ (- (g (+ x dx)) (g x))
             dx)))

      (define (newton-transform g)
        (lambda (x)
          (- x (/ (g x) ((deriv g) x)))))
      (define (newtons-method g guess)
        (fixed-point (newton-transform g) guess))

      (define (average-damp f)
        (lambda (x) (average x (f x))))]

@sicp[
 (newtons-method (cubic 1 2 3) 1)
 (newtons-method (cubic 3 9 81) 1)]

@section{Exercise 1.41}

@sicpnl[
 (define (double f)
   (lambda (x)
     (f (f x))))]

@sicp[
 (((double (double double)) inc) 5)]

@section{Exercise 1.42}

@sicpnl[
 (define (compose f g)
   (lambda (x)
     (f (g x))))]

@sicp[
 ((compose square inc) 6)]

@section{Exercise 1.43}

@sicpnl[
 (define (repeated f times)
   (hyperop f times compose (lambda (x) x)))]

@sicp[
 ((repeated square 2) 5)]

@section{Exercise 1.44}

@sicpnl[
 (define (smooth f)
   (lambda (x)
     (/ (+ (f (- x dx))
           (f x)
           (f (+ x dx)))
        3)))

 (define (smooth-nth f n)
   ((repeated smooth n) f))]

@sicp[
 ((smooth floor) 1)

 ((smooth-nth floor 10) 1)]

@section{Exercise 1.45}

@sicpnl[
 (define (nth-root n x)
   (fixed-point ((repeated average-damp
                           (floor (/ n 2)))
                 (lambda (y)
                   (/ x
                      (fast-exp y (dec n)))))
                1.0))]

@sicp[
 (define (test-nth-root n x)
   (nth-root n
             (fast-exp x n)))

 (test-nth-root 2 2)
 (test-nth-root 3 3)
 (test-nth-root 4 5)
 (test-nth-root 5 4)
 (test-nth-root 6 6)
 (test-nth-root 7 10)
 (test-nth-root 10 10)]

@section{Exercise 1.46}

@sicpnl[
 (define (iterative-improve good-enough?
                            improve)
   (define (try guess)
     (let ((next (improve guess)))
       (if (good-enough? next guess)
           next
           (try next))))
   try)

 (define (diff a b)
   (abs (- a b)))

 (define (sqrt x)
   ((iterative-improve (lambda (guess last)
                         (< (diff (square guess)
                                  x)
                            0.000001))
                       (lambda (guess)
                         (average guess
                                  (/ x
                                     guess)))) 1.0))

 (define (fixed-point f
                      first-guess)
   ((iterative-improve (lambda (guess last)
                         (< (diff guess
                                  last)
                            0.000001))
                       f) first-guess))]

@sicp[
 (sqrt 9)
 (test-nth-root 10 10)]
