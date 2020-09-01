#lang scribble/manual
@(require racket/sandbox
          scribble/example
          scribble-math/dollar
          "../sicp-eval.rkt")

@title[#:style (with-html5 manual-doc-style)]{Symbolic Data}
@(use-katex)

@section{Exercise 2.53}

@sicp[#:label "Copied:"
      (define (memq item x)
        (cond [(null? x) false]
              [(eq? item (car x)) x]
              [else (memq item (cdr x))]))]

@sicpnl[(print-list (list 'a 'b 'c))
        (print-list (list (list 'george)))
        (print-list (cdr '((x1 x2) (y1 y2))))

        (print-list (cadr '((x1 x2) (y1 y2))))
        (pair? (car '(a short list)))
        (memq 'red '((red shoes) (blue socks)))

        (print-list (memq 'red '(red shoes blue socks)))]

@section{Exercise 2.54}

@sicpnl[(define (isequal? a b)
          (if (list? a)
              (and (list? b)
                   (or (and (null? a)
                            (null? b))
                       (and (isequal? (car a)
                                      (car b))
                            (isequal? (cdr a)
                                      (cdr b)))))
              (eq? a b)))]

@sicp[(isequal? '(a (b c) d (e f (g h) i)) '(a (b c) d (e f (g h) i)))
      (isequal? '(a b c) '((a b c)))]

@section{Exercise 2.55}

@tt{'obj} is equivalent to @tt{(quote obj)}.
The abbreviated form is converted into the longer form by the Scheme reader.

Thus, quoting the quoted form is equivalent to @tt{'(quote obj)},
whose @tt{car} is in fact @tt{quote}.

@sicpnl[(print-list ''abracadabra)]

@section{Exercise 2.56}

@sicp[#:label "Copied:"
      (define (variable? x) (symbol? x))

      (define (same-variable? v1 v2)
        (and (variable? v1) (variable? v2) (eq? v1 v2)))

      (define (make-sum a1 a2)
        (cond ((=number? a1 0) a2)
              ((=number? a2 0) a1)
              ((and (number? a1) (number? a2)) (+ a1 a2))
              (else (list '+ a1 a2))))

      (define (make-product m1 m2)
        (cond ((or (=number? m1 0) (=number? m2 0)) 0)
              ((=number? m1 1) m2)
              ((=number? m2 1) m1)
              ((and (number? m1) (number? m2)) (* m1 m2))
              (else (list '* m1 m2))))

      (define (sum? x)
        (and (pair? x) (eq? (car x) '+)))

      (define (addend s) (cadr s))

      (define (augend s) (caddr s))

      (define (product? x)
        (and (pair? x) (eq? (car x) '*)))

      (define (multiplier p) (cadr p))

      (define (multiplicand p) (caddr p))

      (define (=number? exp num)
        (and (number? exp) (= exp num)))]

@sicpnl[(define (exponentiation? x)
          (and (pair? x) (eq? (car x) '**)))

        (define (base e)
          (cadr e))

        (define (exponent e)
          (caddr e))

        (define (make-exponentiation base exponent)
          (cond [(and (number? exponent)
                      (or (= 0 exponent)
                          (= 1 exponent)))
                 (if (= 0 exponent)
                     1
                     base)]
                [(and (number? base)
                      (number? exponent))
                 (expt base exponent)]
                [else (list '** base exponent)]))

        (define (deriv exp var)
          (cond [(number? exp) 0]
                [(variable? exp)
                 (if (same-variable? exp var) 1 0)]
                [(sum? exp)
                 (make-sum (deriv (addend exp) var)
                           (deriv (augend exp) var))]
                [(product? exp)
                 (make-sum
                  (make-product (multiplier exp)
                                (deriv (multiplicand exp) var))
                  (make-product (deriv (multiplier exp) var)
                                (multiplicand exp)))]
                [(exponentiation? exp)
                 (if (same-variable? (base exp) var)
                     (make-product (exponent exp)
                                   (make-exponentiation (base exp)
                                                        (make-sum (exponent exp) -1)))
                     0)]
                (else
                 (error "unknown expression type -- DERIV" exp))))]

@sicp[(print-list (deriv '(+ (** x 2) (** x y)) 'x))]

@section{Exercise 2.57}

@sicpnl[(define (augend s)
          (if (null? (cdddr s))
              (caddr s)
              (cons '+
                    (cddr s))))

        (define (multiplicand p)
          (if (null? (cdddr p))
              (caddr p)
              (cons '*
                    (cddr p))))]

@sicp[(print-list (deriv '(* x y (+ x 3)) 'x))]
