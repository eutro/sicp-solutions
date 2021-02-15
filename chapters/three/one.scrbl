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

@section{Exercise 3.4}

@sicpnl[(define (make-account balance password)
          (define (withdraw amount)
            (if (>= balance amount)
                (begin (set! balance (- balance amount))
                       balance)
                "Insufficient funds"))
          (define (deposit amount)
            (set! balance (+ balance amount))
            balance)
          (define wrong-password
            (let ([attempts-left 7])
              (lambda args
                (if (> attempts-left 0)
                    (begin
                      (set! attempts-left (dec attempts-left))
                      "Incorrect password")
                    (call-the-cops)))))
          (define (dispatch pwd m)
            (cond [(not (eq? pwd password)) wrong-password]
                  [(eq? m 'withdraw) withdraw]
                  [(eq? m 'deposit) deposit]
                  [else (error "Unknown request -- MAKE-ACCOUNT"
                               m)]))
          dispatch)
        (define (call-the-cops)
          (error "The cops have been called"))]

@sicp[(define acc (make-account 100 'secret-password))
      ((acc 'wrong-password 'withdraw) 10)
      ((acc 'wrong-password 'withdraw) 10)
      ((acc 'wrong-password 'withdraw) 10)
      ((acc 'wrong-password 'withdraw) 10)
      ((acc 'wrong-password 'withdraw) 10)
      ((acc 'wrong-password 'withdraw) 10)
      ((acc 'wrong-password 'withdraw) 10)
      ((acc 'wrong-password 'withdraw) 10)]

@section{Exercise 3.5}

@sicp[#:label "Copied:"
      (define (monte-carlo trials experiment)
        (define (iter trials-remaining trials-passed)
          (cond ((= trials-remaining 0)
                 (/ trials-passed trials))
                ((experiment)
                 (iter (- trials-remaining 1) (+ trials-passed 1)))
                (else
                 (iter (- trials-remaining 1) trials-passed))))
        (iter trials 0))
      (define (random-in-range low high)
        (let ((range (- high low)))
          (+ low (random range))))]

@sicpnl[(define (estimate-integral P x1 x2 y1 y2 trials)
          (monte-carlo
           trials
           (lambda ()
             (P (random-in-range x1 x2)
                (random-in-range y1 y2)))))]

@sicp[(define (in-unit-circle? x y)
          (< (+ (square x)
                (square y))
             1))
      (define (estimate-pi trials)
        (* (estimate-integral
            in-unit-circle?
            0.0 1.0
            0.0 1.0
            trials)
           4.0))
      (estimate-pi 10000)
      (estimate-pi 10000)]

@section{Exercise 3.6}

@tt{rand-update} is currently undefined, so we'll define it as @tt{inc}.

@sicpnl[(define rand-update inc)]

Then:

@sicpnl[(define new-rand
          (let ([last 0])
            (lambda (op)
              (cond
                [(eq? op 'generate)
                 (set! last (rand-update last))
                 last]
                [(eq? op 'reset)
                 (lambda (new-last) (set! last new-last))]
                [else
                 (error "Unknown operation -- NEW-RAND"
                        op)]))))]

@sicp[(new-rand 'generate)
      (new-rand 'generate)
      ((new-rand 'reset) 100)
      (new-rand 'generate)]

@section{Exercise 3.7}

@sicpnl[(define (make-joint account account-password joint-password)
          (define wrong-password
            (let ([attempts-left 7])
              (lambda args
                (if (> attempts-left 0)
                    (begin
                      (set! attempts-left (dec attempts-left))
                      "Incorrect password")
                    (call-the-cops)))))
          (define (dispatch pwd m)
            (if (eq? pwd joint-password)
                (account account-password m)
                wrong-password))
          dispatch)]

@sicp[(define peter-acc (make-account 1000 'open-sesame))
      (define paul-acc
        (make-joint peter-acc 'open-sesame 'rosebud))
      ((peter-acc 'open-sesame 'withdraw) 30)
      ((peter-acc 'rosebud 'withdraw) 30)
      ((paul-acc 'open-sesame 'deposit) 20)
      ((paul-acc 'rosebud 'deposit) 20)]

@section{Exercise 3.8}

@sicpnl[(define (order-matters)
          (let ([last 1])
            (lambda (n)
              (let ([ret
                     (if (= last n)
                         n
                         0)])
                (set! last n)
                ret))))]

@sicp[(let* ([f (order-matters)]
             [x (f 0)]
             [y (f 1)])
        (+ x y))
      (let* ([f (order-matters)]
             [y (f 1)]
             [x (f 0)])
        (+ x y))]
