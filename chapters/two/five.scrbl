#lang scribble/manual
@(require racket/sandbox
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
