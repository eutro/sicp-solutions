#lang scribble/manual
@(require racket/sandbox
          scribble/example
          scribble-math/dollar
          "../sicp-eval.rkt")

@title[#:style (with-html5 manual-doc-style)]{The Elements of Programming}
@(use-katex)

@section{Exercise 1.1}

@sicp[#:label "Just evaluate in Scheme:"
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
@sicp[#:label #f
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

@sicpnl[
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
@sicp[#:label "These were already defined:"
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
@sicpnl[
 (good-enough? 0 0.0001)]

This works poorly for large numbers, as it may not be possible to represent a number precisely enough to pass the threshold.

For example, numbers between @tt{3037000499.97605} and @tt{3037000499.9760494} cannot be represented:

@sicpnl[
 (define number 9223372036854775807)
 (define below-root 3037000499.9760494)
 (define above-root 3037000499.97605)
 (average below-root above-root)
 (= below-root (average below-root above-root))]

Yet neither satisfies @tt{good-enough?}:
@sicpnl[
 (good-enough? below-root number)
 (- (square below-root) number)]

@sicpnl[
 (good-enough? above-root number)
 (- (square above-root) number)]

@sicp[#:label "Thus, a more suitable loop may be:"
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

@sicp[
 (sqrt+ 9223372036854775807)
 (square (sqrt+ 9223372036854775807))]

@sicpnl[
 (sqrt+ 0.0001)
 (square (sqrt+ 0.0001))]

@section{Exercise 1.8}
Here is a cube root operator implemented using the previous @tt{good-enough?} test.
@sicpnl[ ;; I don't care, I'm doing it generally now
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

@sicp[
 (cbrt 8)
 (cbrt 68719476736)]


