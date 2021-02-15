#lang scribble/manual
@(require racket/sandbox
          scribble/example
          scribble-math/dollar
          "../sicp-eval.rkt")

@title[#:style (with-html5 manual-doc-style)]{Assignment and Local State}
@(use-katex)

@section{Exercise 3.1}

@sicpnl[(define (make-accumulator total)
          (lambda (value)
            (set! total (+ total value))
            total))]

@sicp[(define A (make-accumulator 5))
      (A 10)
      (A 10)]

@section{Exercise 3.2}

@sicpnl[(define (make-monitored f)
          (let ([called 0])
            (lambda (arg)
              (cond
                [(eq? arg 'how-many-calls?) called]
                [(eq? arg 'reset-count) (set! called 0)]
                [else
                 (set! called (inc called))
                 (f arg)]))))]

@sicp[(define s (make-monitored sqrt))
      (s 100)
      (s 'how-many-calls?)
      (s 400)
      (s 'how-many-calls?)
      (s 'reset-count)
      (s 4)
      (s 'how-many-calls?)]

@section{Exercise 3.3}

@sicpnl[(define (make-account balance password)
          (define (withdraw amount)
            (if (>= balance amount)
                (begin (set! balance (- balance amount))
                       balance)
                "Insufficient funds"))
          (define (deposit amount)
            (set! balance (+ balance amount))
            balance)
          (define (wrong-password . args)
            "Incorrect password")
          (define (dispatch pwd m)
            (cond [(not (eq? pwd password)) wrong-password]
                  [(eq? m 'withdraw) withdraw]
                  [(eq? m 'deposit) deposit]
                  [else (error "Unknown request -- MAKE-ACCOUNT"
                               m)]))
          dispatch)]

@sicp[(define acc (make-account 100 'secret-password))
      ((acc 'secret-password 'withdraw) 40)
      ((acc 'some-other-password 'deposit) 50)]
