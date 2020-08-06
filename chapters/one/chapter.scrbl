#lang scribble/manual
@(require racket/sandbox
          scribble/example
          scribble-math/dollar)
@(define sicp-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
     (make-evaluator 'sicp)))

@title[#:style (with-html5 manual-doc-style)]{Chapter One}
@(use-katex)

@section{Exercise 1.1}

@examples[#:eval sicp-evaluator #:label "Just evaluate in Scheme:"
          10
          (+ 5 3 4)
          (- 9 1)
          (/ 6 2)
          (+ (* 2 4) (- 4 6))
          (define a 3)
          (define b (+ a 1))
          (+ a b (* a b))
          (= a b)
          (if (and (> b a) (< b (* a b))) b a)
          (cond ((= a 4) 6) ((= b 4) (+ 6 7 a)) (else 25))
          (+ 2 (if (> b a) b a))
          (* (cond ((> a b) a) ((< a b) b) (else -1)) (+ a 1))]

@section{Exercise 1.2}
@$${5+1+(2-(3-(6+{1\over3})))\over3(6-2)(2-7)}

Written in Scheme is:
@examples[#:eval sicp-evaluator #:label #f
          (/ (+ 5
                1
                (- 2
                   (- 3
                      (+ 6
                         (/ 1 3)))))
             (* 3
                (- 6 2)
                (- 2 7)))]

@section{Exercise 1.3}

@examples[#:eval sicp-evaluator #:label #f
          (define (sum-of-larger-squares a b c) (cond ((and (< a b) (< a c)) (+ (* b b) (* c c)))
                                                      ((and (< b a) (< b c)) (+ (* a a) (* c c)))
                                                      (else (+ (* a a) (* b b)))))]
          
@examples[#:eval sicp-evaluator
          (sum-of-larger-squares 1 2 3)
          (sum-of-larger-squares 2 3 1)
          (sum-of-larger-squares 3 1 2)]

@section{Exercise 1.4}
This procedure takes two parameters, @${a} and @${b}, and returns @${a + |b|}.
That is, if @${b > 0}, it returns the sum: @${a + b},
otherwise it returns the difference: @${a - b}.

@section{Exercise 1.5}
With applicative-order evaluation, @racket[(p)] never gets evaluated, as @racket[(= x 0)]
evaluates to true, with the procedure returning 0 without having to evaluate the second parameter.

However, with normal-order evaluation, all sub-expressions are to be evaluated first, and then reduced.
@racket[(p)] cannot be evaluated in this way, since it is defined entirely recursively, thus the evaluation will fail or hang as it is stuck in an infinite loop.

@section{Exercise 1.6}
Before being passed to @tt{new-if}, both sub-expressions are evaluated. This causes
@tt{sqrt-iter} to invoke itself endlessly, causing the program to hang or overflow the stack.

@section{Exercise 1.7}
@examples[#:eval sicp-evaluator #:label "These were already defined:"
          (define (square x) (* x x))
          (define (average x y)
            (/ (+ x y) 2))
          (define (improve guess x)
            (average guess (/ x guess)))
          (define (good-enough? guess x)
            (< (abs (- (square guess) x)) 0.001))
          (define (sqrt-iter guess x)
            (if (good-enough? guess x)
                guess
                (sqrt-iter (improve guess x) x)))]

This works poorly for small numbers, as the allowed error % is inversely proportional to the input:
@examples[#:eval sicp-evaluator #:label #f
          (good-enough? 0 0.0001)]

This works poorly for large numbers, as it may not be possible to represent a number precisely enough to pass the threshold.

For example, numbers between @tt{3037000499.97605} and @tt{3037000499.9760494} cannot be represented:

@examples[#:eval sicp-evaluator #:label #f
          (define number 9223372036854775807)
          (define below-root 3037000499.9760494)
          (define above-root 3037000499.97605)
          (average below-root above-root)
          (= below-root (average below-root above-root))]

Yet neither satisfies @tt{good-enough?}:
@examples[#:eval sicp-evaluator #:label #f
          (good-enough? below-root number)
          (- (square below-root) number)]

@examples[#:eval sicp-evaluator #:label #f
          (good-enough? above-root number)
          (- (square above-root) number)]

@examples[#:eval sicp-evaluator #:label "Thus, a more suitable loop may be:"
          (define (good-enough+? guess last-guess)
            (< (/ (abs (- guess
                          last-guess))
                  guess)
               0.001))
          (define (sqrt-iter+ guess last-guess x)
            (if (good-enough+? guess last-guess)
                guess
                (sqrt-iter+ (improve guess x) guess x)))
          (define (sqrt+ x)
            (sqrt-iter+ 1.0 0.0 x))]

@examples[#:eval sicp-evaluator
          (sqrt+ 9223372036854775807)
          (square (sqrt+ 9223372036854775807))]

@examples[#:eval sicp-evaluator #:label #f
          (sqrt+ 0.0001)
          (square (sqrt+ 0.0001))]

@section{Exercise 1.8}
Here is a cube root operator implemented using the previous @tt{good-enough?} test.
@examples[#:eval sicp-evaluator #:label #f ;; I don't care, I'm doing it generally now
          (define (newton procedure
                          improve)
            (define (good-enough? guess x) ;; I'll use the old "good-enough?" test
              (< (abs (- (procedure guess) x)) 0.001))
            (define (iter guess x)
              (if (good-enough? guess x)
                  guess
                  (iter (improve guess x) x)))
            (lambda (x) (iter 1.0 x)))
          
          (define cbrt (newton (lambda (x)
                                 (* x x x))
                               (lambda (y x)
                                 (/ (+ (/ x
                                          (* y y))
                                       (* 2 y))
                                    3))))]

@examples[#:eval sicp-evaluator
          (cbrt 8)
          (cbrt 68719476736)]

@section{Exercise 1.9}

The first, recursive @tt{+}.
@racketblock[
 (+ 4 5)
 (inc (+ 3 5))
 (inc (inc (+ 2 5)))
 (inc (inc (inc (+ 1 5))))
 (inc (inc (inc (inc (+ 0 5)))))
 (inc (inc (inc (inc 5))))
 (inc (inc (inc 6)))
 (inc (inc 7))
 (inc 8)
 9]

The second, iterative @tt{+}.
@racketblock[
 (+ 4 5)
 (+ 3 6)
 (+ 2 7)
 (+ 1 8)
 (+ 0 9)
 9]

@section{Exercise 1.10}
@examples[#:eval sicp-evaluator #:label #f
          (define (A x y)
            (cond ((= y 0) 0)
                  ((= x 0) (* 2 y))
                  ((= y 1) 2)
                  (else (A (- x 1)
                           (A x (- y 1))))))]

@examples[#:eval sicp-evaluator #:label "Here we just evaluate the examples:"
          (A 1 10)
          (A 2 4)
          (A 3 3)]

These procedures:

@racketblock[
 (define (f n) (A 0 n))
 (define (g n) (A 1 n))
 (define (h n) (A 2 n))]

Can be written as such:
@$${f(n) = 2n}
@$${g(n) = 2^n}
@$${h(n) = 2 \uparrow\uparrow n}

Where @${\uparrow\uparrow} denotes @(hyperlink "https://en.wikipedia.org/wiki/Tetration" "tetration").

Or, using @${[n]} for general @(hyperlink "https://en.wikipedia.org/wiki/Hyperoperation" "hyperoperation"):
@$${f(n) = 2[2]n}
@$${g(n) = 2[3]n}
@$${h(n) = 2[4]n}

For positive integer values of @tt{n}.

@section{Exercise 1.11}

@tt{f} recursively:
@examples[#:eval sicp-evaluator #:label #f
          (define (f n)
            (if (< n 3)
                n
                (+ (f (- n 1))
                   (* 2 (f (- n 2)))
                   (* 3 (f (- n 3))))))]

@examples[#:eval sicp-evaluator
          (f 1)
          (f 2)
          (f 3)
          (f 5)
          (f 7)
          (f 10)]

@tt{f} iteratively:
@examples[#:eval sicp-evaluator #:label #f
          (define (f n)
            (if (< n 3)
                n
                (f-iter 0 1 2 n)))
          
          (define (f-iter a b c count)
            (if (< count 3)
                c
                (f-iter b
                        c
                        (+ c
                           (* 2 b)
                           (* 3 a))
                        (dec count))))]

@examples[#:eval sicp-evaluator
          (f 1)
          (f 2)
          (f 3)
          (f 5)
          (f 7)
          (f 10)]

@section{Exercise 1.12}
@examples[#:eval sicp-evaluator #:label #f
          (define (pascal row index)
            (if (or (= index 0)
                    (>= index row))
                1
                (+ (pascal (dec row)
                           (dec index))
                   (pascal (dec row)
                           index))))]

@examples[#:eval sicp-evaluator
          (display (list (pascal 0 0)))
          (display (list (pascal 1 0) (pascal 1 1)))
          (display (list (pascal 2 0) (pascal 2 1) (pascal 2 2)))
          (display (list (pascal 3 0) (pascal 3 1) (pascal 3 2) (pascal 3 3)))
          (display (list (pascal 4 0) (pascal 4 1) (pascal 4 2) (pascal 4 3)(pascal 4 4)))]

@section{Exercise 1.13}
@${Fib(n)} is defined as:
@$${Fib(0) = 0}
@$${Fib(1) = 1}
@$${Fib(n) = Fib(n-1) + Fib(n-2)}

Let
@${\phi={1+\sqrt{5}\over2}}
and
@${\psi={1-\sqrt{5}\over2}}.

Let's assume:
@$${Fib(n) = {\phi^n - \psi^n \over \sqrt{5}}}
Which holds true for @${n = 0} and @${n = 1}:
@$${Fib(0) = {\phi^0 - \psi^0 \over \sqrt{5}} = 0}
@$${Fib(1) = {\phi^1 - \psi^1 \over \sqrt{5}} = 1}

Using the other definition of @${Fib(n)}:
@$${Fib(n) = {{\phi^{n-1} - \psi^{n-1} \over \sqrt{5}} + {\phi^{n-2} - \psi^{n-2} \over \sqrt{5}}}}

Since @${\psi} and @${\phi} both satisfy the equation
@${x^2 = x + 1},
they also both satisfy the equation
@${x^n = x^{n-1} x^{n-2}}.

Thus,
@$${Fib(n) = {{(\phi^{n-1} + \phi^{n-2}) - (\psi^{n-1} + \psi^{n-2}) \over \sqrt{5}}}}
can be simplified to
@$${Fib(n) = {\phi^n - \psi^n \over \sqrt{5}}}

Thus, by induction, it holds true for all positive integers @${n}.

It can be rearranged as such:
@$${{\phi^n \over \sqrt{5}} - Fib(n) = {\psi^n \over \sqrt{5}}}

Since @${-1 < \psi < 1}, for all positive @${n}:
@$${|\psi^n| < |\psi|}
@$${|{\psi^n \over \sqrt{5}}| < 0.5}
@$${|{\phi^n \over \sqrt{5}} - Fib(n)| < 0.5}
And thus @${Fib(n)} is the closest integer to @${\phi^n \over \sqrt{5}}.

@section{Exercise 1.14}
@image["chapters/one/asymptote-images/fb44e5436c9fd8cd1af1bb2b4b8337da"
       #:scale 0.35
       #:suffixes (list ".svg"
                        ".png"
                        ".pdf")]

@section{Exercise 1.15}

@subsection{Exercise 1.15.a}
@tt{p} is applied five times:
@examples[#:eval sicp-evaluator #:label #f
          (define (cube x) (* x x x))
          (define (p x)
            (begin (display "p called\n")
                   (- (* 3 x) (* 4 (cube x)))))
          (define (sine angle)
            (if (not (> (abs angle) 0.1))
                angle
                (p (sine (/ angle 3.0)))))
          (sine 12.5)]

@subsection{Exercise 1.15.b}
The order of growth, for both space and number of steps, is:
@$${log(a)}

@section{Exercise 1.16}
@examples[#:eval sicp-evaluator #:label "Defining abstract hyperoperation:"
          (define (hyperop base exponent op identity)
            (define (hyperop-iter state base exponent)
              (cond ((= exponent 0) state)
                    ((even? exponent) (hyperop-iter state
                                                    (op base base)
                                                    (/ exponent 2)))
                    (else (hyperop-iter (op state base)
                                        base
                                        (dec exponent)))))
            (define (even? n)
              (= (remainder n 2) 0))
            
            (hyperop-iter identity base exponent))]

@examples[#:eval sicp-evaluator #:label "Exponentiation:"
          (define (fast-exp a b) (hyperop a b * 1))
          
          (fast-exp 10 10)
          (fast-exp 3 3)
          (fast-exp 2 16)]

@section{Exercise 1.17}
@examples[#:eval sicp-evaluator #:label "Multiplication:"
          (define (fast-mult a b) (hyperop a b + 0))
          
          (fast-mult 10 10)
          (fast-mult 3 3)
          (fast-mult 2 16)]

@section{Exercise 1.18}
what

@section{Exercise 1.19}
@examples[#:eval sicp-evaluator #:label "Fibbonaci"
          (define (fib n)
            (fib-iter 1 0 0 1 n))
          (define (fib-iter a b p q count)
            (cond ((= count 0) b)
                  ((even? count)
                   (fib-iter a
                             b
                             (+ (square p) (square q))
                             (+ (* (+ p q) q)
                                (* q p))
                             (/ count 2)))
                  (else (fib-iter (+ (* b q) (* a q) (* a p))
                                  (+ (* b p) (* a q))
                                  p
                                  q
                                  (- count 1)))))]

@examples[#:eval sicp-evaluator
          (fib 3)
          (fib 15)
          (fib 63)]

@section{Exercise 1.20}
@racketblock[
 (gcd 206 40)
 (gcd 40 (remainder 206 40))
 (gcd 40 6)
 (gcd 6 (remainder 40 6))
 (gcd 6 4)
 (gcd 4 (remainder 6 4))
 (gcd 4 2)
 (gcd 2 (remainder 4 2))
 (gcd 2 0)
 2]

@section{Exercise 1.21}
@examples[#:eval sicp-evaluator #:label "Just define it as in the book..."
          (define (smallest-divisor n)
            (define (find-divisor n test-divisor)
              (cond ((> (square test-divisor) n) n)
                    ((divides? test-divisor n) test-divisor)
                    (else (find-divisor n (+ test-divisor 1)))))
            (define (divides? a b)
              (= (remainder b a) 0))
            (find-divisor n 2))]

@examples[#:eval sicp-evaluator #:label "And evaluate:"
          (smallest-divisor 199)
          (smallest-divisor 1999)
          (smallest-divisor 19999)]

@section{Exercise 1.22}
@examples[#:eval sicp-evaluator #:label "Boilerplate from the book..."
          (define (timed-prime-test n)
            (define result (start-prime-test n (runtime)))
            (if result
                (begin (newline)
                       (display n)
                       (display result))
                #f))
          (define (start-prime-test n start-time)
            (if (prime? n)
                (report-prime (- (runtime) start-time))
                #f))
          (define (report-prime elapsed-time)
            (string-append " *** " (number->string elapsed-time)))
          (define (prime? n)
            (= n (smallest-divisor n)))]

@examples[#:eval sicp-evaluator #:label #f
          (define (search-for-primes start)
            (define (iter i count)
              (if (< count 3)
                  (iter (+ i 2)
                        (if (timed-prime-test i)
                            (inc count)
                            count))))
            (iter start 0))]

@examples[#:eval sicp-evaluator
          (search-for-primes 100001)
          (search-for-primes 1000001)
          (search-for-primes 10000001)
          (search-for-primes 100000001)]

@section{Exercise 1.23}
@examples[#:eval sicp-evaluator #:label #f
          (define (smallest-divisor n)
            (define (find-divisor n test-divisor)
              (cond ((> (square test-divisor) n) n)
                    ((divides? test-divisor n) test-divisor)
                    (else (find-divisor n (next test-divisor)))))
            (define (divides? a b)
              (= (remainder b a) 0))
            (define (next x)
              (if (= x 2)
                  3
                  (+ x 2)))
            (find-divisor n 2))]

@examples[#:eval sicp-evaluator
          (search-for-primes 100001)
          (search-for-primes 1000001)
          (search-for-primes 10000001)
          (search-for-primes 100000001)]

The expectation is not entirely correct, since the @tt{if} statement requires calculation as well.

@section{Exercise 1.24}
@examples[#:eval sicp-evaluator #:label "From the book:"
          (define (expmod base exp m)
            (cond ((= exp 0) 1)
                  ((even? exp)
                   (remainder (square (expmod base (/ exp 2) m))
                              m))
                  (else
                   (remainder (* base (expmod base (- exp 1) m))
                              m))))
          (define (fermat-test n)
            (define (try-it a)
              (= (expmod a n n) a))
            (try-it (+ 1 (random (- n 1)))))
          (define (fast-prime? n times)
            (cond ((= times 0) true)
                  ((fermat-test n) (fast-prime? n (- times 1)))
                  (else false)))]

@examples[#:eval sicp-evaluator #:label #f
          (define (start-prime-test n start-time)
            (if (fast-prime? n 3)
                (let ((time (- (runtime) start-time)))
                  (if (prime? n)
                      (string-append (report-prime time)
                                     " <- true positive")
                      (begin (newline)
                             (display n)
                             (display (report-prime time))
                             (display " <- false positive")
                             #f)))
                #f))]

@examples[#:eval sicp-evaluator
          (search-for-primes 100001)
          (search-for-primes 1000001)
          (search-for-primes 10000001)
          (search-for-primes 100000001)]

The times are expected to be much better, and scale much slower, than those in the previous exercises.

@section{Exercise 1.25}
Alyssa is incorrect. @tt{(exp a b)} requires @${a^b} to be calculated, which can get very large and be irrepresentable.
However, @tt{expmod} is not tail-recursive, and thus must retain all sub-computations in memory.

@section{Exercise 1.26}
The given @tt{expmod} procedure calls at most one more @tt{expmod} itself, passing it to @tt{square} having calculated it once,
while Louis' calculates it twice, multiplying them together, wasting time.

@section{Exercise 1.27}

@examples[#:eval sicp-evaluator #:label #f
          (define (all-congruent? number)
            (define (is-congruent? a n)
              (= (expmod a n n) a))
            (define (iter tested)
              (cond ((= 1 tested) #t)
                    ((is-congruent? tested number) (iter (dec tested)))
                    (else #f)))
            (iter (dec number)))]

@examples[#:eval sicp-evaluator #:label "Some expected results:"
          (all-congruent? 100)
          (all-congruent? 113)
          (all-congruent? 560)]

@examples[#:eval sicp-evaluator #:label "Some Carmichael numbers:"
          (all-congruent? 561)
          (all-congruent? 1105)
          (all-congruent? 1729)]

@section{Exercise 1.28}

@examples[#:eval sicp-evaluator #:label #f
          (define (miller-rabin n times)
            (define n-1 (dec n))
            (define (do-test a)
              (= 1 (expmod a n-1 n)))
            (define (expmod base exp m)
              (define (mulmod x y)
                (define rem (remainder (* x y) m))
                (if (and (= x y)
                         (= rem 1)
                         (not (= x 1))
                         (not (= x (dec m))))
                    0
                    rem))
              (hyperop base exp mulmod 1))
            (define (loop i)
              (cond ((= i 0) #t)
                    ((do-test (inc (random (dec n)))) (loop (dec i)))
                    (else #f)))
            (loop times))]

@examples[#:eval sicp-evaluator
          (miller-rabin 1105 10)
          (miller-rabin 5 10)
          (miller-rabin 23 10)
          (miller-rabin 113 10)]

@section{Exercise 1.29}

Firstly, define @tt{sum} and @tt{integral} as in the book:
@examples[#:eval sicp-evaluator #:label #f
          (define (sum term a next b)
            (if (> a b)
                0
                (+ (term a)
                   (sum term (next a) next b))))

          (define (integral f a b dx)
            (define (add-dx x) (+ x dx))
            (* (sum f (+ a (/ dx 2.0)) add-dx b)
               dx))]

Then, define the @tt{simpson} procedure:
@examples[#:eval sicp-evaluator #:label #f
          (define (simpson f a b n)
            (define h (/ (- b a)
                         n))
            (define (y k)
              (f (+ a
                    (* k h))))
            (* (/ h 3)
               (+ (y 0)
                  (sum (lambda (i)
                         (* (if (even? i)
                                2
                                (/ 1 2))
                            (y i)))
                       1
                       inc
                       (dec n))
                  (y n))))]

@examples[#:eval sicp-evaluator
          (integral cube 0 1 0.01)
          (simpson cube 0 1 100)
          (integral cube 0 1 0.001)
          (simpson cube 0 1 1000)]

@section{Exercise 1.30}

@examples[#:eval sicp-evaluator #:label #f
          (define (sum term a next b)
            (define (iter a result)
              (if (> a b)
                  result
                  (iter (next a) (+ result (term a)))))
            (iter a 0))]

@examples[#:eval sicp-evaluator
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

@examples[#:eval sicp-evaluator #:label "Recursive:"
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

@examples[#:eval sicp-evaluator #:label "Factorial:"
          (define (factorial x)
            (product identity 1 inc x))

          (factorial 6)]

@examples[#:eval sicp-evaluator #:label "Pi:"
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

@examples[#:eval sicp-evaluator #:label "Iterative:"
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

@examples[#:eval sicp-evaluator #:label #f
          (define (accumulate combiner null-value term a next b)
            ((accumulator-recur combiner null-value) term a next b))

          (define (sum term a next b)
            (accumulate + 0 term a next b))

          (define (product term a next b)
            (accumulate * 1 term a next b))]

@examples[#:eval sicp-evaluator
          (sum-integers 1 10)
          (factorial 6)]

@subsection{Exercise 1.32.b}

@examples[#:eval sicp-evaluator #:label #f
          (define (accumulate combiner null-value term a next b)
            ((accumulator-iter combiner null-value) term a next b))]

@examples[#:eval sicp-evaluator
          (sum-integers 1 10)
          (factorial 6)]

Currying is cooler though.

@section{Exercise 1.33}

@examples[#:eval sicp-evaluator #:label #f
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

@examples[#:eval sicp-evaluator #:label #f
          (define (mr-prime? x) (miller-rabin x 10))

          (define (sum-prime-cubes a b)
            (filtered-accumulate mr-prime?
                                 +
                                 0
                                 cube
                                 a
                                 inc
                                 b))]

@examples[#:eval sicp-evaluator
          (sum-prime-cubes 2 5)]

@subsection{Exercise 1.33.b}

@examples[#:eval sicp-evaluator #:label #f
          (define (product-relative-primes n)
            (filtered-accumulate (lambda (x)
                                   (= 1 (gcd x n)))
                                 *
                                 1
                                 identity
                                 1
                                 inc
                                 (dec n)))]

@examples[#:eval sicp-evaluator
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
@examples[#:eval sicp-evaluator #:label #f
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
@examples[#:eval sicp-evaluator #:label #f
          (fixed-point (lambda (x) (+ 1
                                      (/ 1 x)))
                       1.0)]

@section{Exercise 1.36}

@examples[#:eval sicp-evaluator #:label #f
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

@examples[#:eval sicp-evaluator #:label "Without average damping:"
          (fixed-point-logged (lambda (x)
                                (/ (log 1000)
                                   (log x)))
                              5)]

@examples[#:eval sicp-evaluator #:label "With average damping:"
          (fixed-point-logged (lambda (x)
                                (average x
                                         (/ (log 1000)
                                            (log x))))
                              5)]

@section{Exercise 1.37}

@subsection{Exercise 1.37.a}

@examples[#:eval sicp-evaluator #:label "Recursive:"
          (define (cont-frac N D k)
            (define (recur i)
              (if (> i k)
                  0
                  (/ (N i)
                     (+ (D i) (recur (inc i))))))
            (recur 1))]

@examples[#:eval sicp-evaluator
          (define (approx-phi k)
            (/ 1
               (cont-frac (lambda (i) 1.0)
                          (lambda (i) 1.0)
                          k)))

          (approx-phi 13)]

@${k = 13} results in an estimate correct to four decimal places.

@subsection{Exercise 1.37.b}

@examples[#:eval sicp-evaluator #:label "Iterative:"
          (define (cont-frac N D k)
            (define (iter i result)
              (if (< i 1)
                  result
                  (iter (dec i)
                        (/ (N i)
                           (+ (D i) result)))))
            (iter k 0))]

@examples[#:eval sicp-evaluator
          (approx-phi 13)]

@section{Exercise 1.38}

@examples[#:eval sicp-evaluator #:label #f
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

@examples[#:eval sicp-evaluator
          (approx-e 1000)]

@section{Exercise 1.39}

@examples[#:eval sicp-evaluator #:label #f
          (define (tan-cf x k)
              (cont-frac (let ((-x^2 (- 0 (square x))))
                           (lambda (i)
                             (if (= i 1)
                                 x
                                 -x^2)))
                         (lambda (i)
                           (dec (* 2 i)))
                         k))]

@examples[#:eval sicp-evaluator
          (let ((pi 3.141593))
            (tan-cf (/ pi 4)
                    100))]

@section{Exercise 1.40}

@examples[#:eval sicp-evaluator #:label #f
          (define (cubic a b c)
            (lambda (x)
              (+ (cube x)
                 (* a
                    (square x))
                 (* b
                    x)
                 c)))]

@examples[#:eval sicp-evaluator #:label "Some copying from the book...:"
          (define dx 0.00001)

          (define (deriv g)
            (lambda (x)
              (/ (- (g (+ x dx)) (g x))
                 dx)))

          (define (newton-transform g)
            (lambda (x)
              (- x (/ (g x) ((deriv g) x)))))
          (define (newtons-method g guess)
            (fixed-point (newton-transform g) guess))]

@examples[#:eval sicp-evaluator
          (newtons-method (cubic 1 2 3) 1)
          (newtons-method (cubic 3 9 81) 1)]

@section{Exercise 1.41}

@examples[#:eval sicp-evaluator #:label #f
          (define (double f)
            (lambda (x)
              (f (f x))))]

@examples[#:eval sicp-evaluator
          (((double (double double)) inc) 5)]

@section{Exercise 1.42}

@examples[#:eval sicp-evaluator #:label #f
          (define (compose f g)
            (lambda (x)
              (f (g x))))]

@examples[#:eval sicp-evaluator
          ((compose square inc) 6)]

@section{Exercise 1.43}

@examples[#:eval sicp-evaluator #:label #f
          (define (repeated f times)
            (hyperop f times compose (lambda (x) x)))]

@examples[#:eval sicp-evaluator
          ((repeated square 2) 5)]
