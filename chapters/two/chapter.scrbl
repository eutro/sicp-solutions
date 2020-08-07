#lang scribble/manual
@(require racket/sandbox
          scribble/example
          scribble-math/dollar)
@(define sicp-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
     (make-evaluator 'sicp)))

@title[#:style (with-html5 manual-doc-style)]{Chapter Two}
@(use-katex)

@section{Exercise 2.1}

@examples[#:eval sicp-evaluator #:label "Some copying..."
          (define numer car)
          (define denom cdr)

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
          (define (equal-rat? x y)
            (= (* (numer x) (denom y))
               (* (numer y) (denom x))))

          (define (print-rat x)
            (display (numer x))
            (display "/")
            (display (denom x))
            (newline))]

Then to define @tt{make-rat}:
@examples[#:eval sicp-evaluator #:label #f
          (define (make-rat n d)
            (cond [(= d 0) (error "Division by 0")]
                  [(< d 0) (make-rat (- 0 n)
                                     (- 0 d))]
                  [else (let ([g (gcd n d)])
                          (cons (/ n g)
                                (/ d g)))]))]

@examples[#:eval sicp-evaluator
          (print-rat (make-rat 6 9))
          (print-rat (make-rat -10 -100))
          (print-rat (make-rat 10 -12))]

@section{Exercise 2.2}

@examples[#:eval sicp-evaluator #:label #f
          (define make-point cons)
          (define x-point car)
          (define y-point cdr)

          (define (print-point p)
            (display "(")
            (display (x-point p))
            (display ",")
            (display (y-point p))
            (display ")")
            (newline))

          (define make-segment cons)
          (define start-segment car)
          (define end-segment cdr)

          (define (average a b)
            (/ (+ a b)
               2))

          (define (midpoint-segment s)
            (make-point (average (x-point (start-segment s))
                                 (x-point (end-segment s)))
                        (average (y-point (start-segment s))
                                 (y-point (end-segment s)))))]

@examples[#:eval sicp-evaluator
          (let ([segment (make-segment (make-point 0 1)
                                       (make-point 3 5))])
            (print-point (start-segment segment))
            (print-point (midpoint-segment segment))
            (print-point (end-segment segment)))]
