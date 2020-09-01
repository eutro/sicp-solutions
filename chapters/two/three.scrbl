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

@section{Exercise 2.58}

@subsection{Exercise 2.58.a}

@sicpnl[(define (make-sum a1 a2)
          (cond ((=number? a1 0) a2)
                ((=number? a2 0) a1)
                ((and (number? a1) (number? a2)) (+ a1 a2))
                (else (list a1 '+ a2))))

        (define (make-product m1 m2)
          (cond ((or (=number? m1 0) (=number? m2 0)) 0)
                ((=number? m1 1) m2)
                ((=number? m2 1) m1)
                ((and (number? m1) (number? m2)) (* m1 m2))
                (else (list m1 '* m2))))

        (define (sum? x)
          (and (pair? x) (eq? (cadr x) '+)))

        (define (addend s) (car s))

        (define (augend s) (caddr s))

        (define (product? x)
          (and (pair? x) (eq? (cadr x) '*)))

        (define (multiplier p) (car p))

        (define (multiplicand p) (caddr p))]

@sicp[(deriv '(x + (3 * (x + (y + 2)))) 'x)
      (deriv '(x + (3 * (x + (y + 2)))) 'y)]

@subsection{Exercise 2.58.b}

@sicpnl[(define order-of-operands ;; Order of operands, from last applied (+/-) to first (**).
          (list (lambda (sym)
                  (or (eq? sym '+)
                      (eq? sym '-)))
                (lambda (sym)
                  (or (eq? sym '*)
                      (eq? sym '/)))
                (partial eq? '**)))

        (define (last-operand exp)
          (fold-left (lambda (found matches?)
                       (if (null? found)
                           (fold-left (lambda (found operand)
                                        (if (null? found)
                                            (if (matches? operand)
                                                operand
                                                found)
                                            found))
                                      nil
                                      exp)
                           found))
                     nil
                     order-of-operands))

        (define (split-on exp sym)
          (if (eq? (car exp) sym)
              (list '() (cdr exp))
              (let ([next-split (split-on (cdr exp)
                                          sym)])
                (list (cons (car exp)
                            (car next-split))
                      (cadr next-split)))))

        (define (sum? x)
          (and (pair? x)
               (eq? (last-operand x)
                    '+)))

        (define (addend s)
          (let ([candidate (car (split-on s '+))])
            (if (null? (cdr candidate))
                (car candidate)
                candidate)))

        (define (augend s)
          (let ([candidate (cadr (split-on s '+))])
            (if (null? (cdr candidate))
                (car candidate)
                candidate)))

        (define (product? x)
          (and (pair? x)
               (eq? (last-operand x) '*)))

        (define (multiplier p)
          (let ([candidate (car (split-on p '*))])
            (if (null? (cdr candidate))
                (car candidate)
                candidate)))

        (define (multiplicand p)
          (let ([candidate (cadr (split-on p '*))])
            (if (null? (cdr candidate))
                (car candidate)
                candidate)))]

@sicp[(deriv '(x + 3 * (x + y + 2)) 'x)
      (deriv '(x + 3 * (x + y + 2)) 'y)]

@section{Exercise 2.59}

@sicp[#:label "Copied:"
      (define (element-of-set? x set)
        (cond ((null? set) false)
              ((equal? x (car set)) true)
              (else (element-of-set? x (cdr set)))))

      (define (adjoin-set x set)
        (if (element-of-set? x set)
            set
            (cons x set)))

      (define (intersection-set set1 set2)
        (cond ((or (null? set1) (null? set2)) '())
              ((element-of-set? (car set1) set2)
               (cons (car set1)
                     (intersection-set (cdr set1) set2)))
              (else (intersection-set (cdr set1) set2))))]

Set printing can be the same as list printing but with braces instead of parentheses:

@sicpnl[(define (set-elements set) set)

        (define (print-set set)
          (display "{")
          (let ([enumerated (set-elements set)])
            (if (null? enumerated)
                '()
                (begin (display (car enumerated))
                       (for-each (lambda (el)
                                   (display " ")
                                   (display el))
                                 (cdr enumerated)))))
          (display "}"))]

@sicpnl[(define union-set
          (partial fold-right adjoin-set))]

@sicp[(print-set (union-set '(a b c) '(b c d)))]

@section{Exercise 2.60}

@sicpnl[(define adjoin-set cons)
        (define union-set append)]

@tt{element-of-set?} and @tt{intersection-set} can be kept the same.

@sicp[#:label "Element enumeration must be redefined though."
      (define (set-elements set)
        (fold-left (lambda (existing next)
                     (if (element-of-set? next existing)
                         existing
                         (cons next existing)))
                   nil
                   set))]

@sicp[(print-set (adjoin-set 'd '(a b c)))
      (print-set (union-set '(a b c) '(b c d)))
      (print-set (intersection-set (union-set '(a b c)
                                              '(a d e))
                                   (union-set '(a b e)
                                              '(a c d))))]

@sicp[#:label "Printing them as lists instead:"
      (print-list (adjoin-set 'd '(a b c)))
      (print-list (union-set '(a b c) '(b c d)))
      (print-list (intersection-set (union-set '(a b c)
                                               '(a d e))
                                    (union-set '(a b e)
                                               '(a c d))))]

This implementation does not provide anything over using a
list instead.

While adding to the set is very fast, checking whether the set
contains an element is slower, as there may be duplicates.

@section{Exercise 2.61}

This implementation is @${O(n)} at most, and @${O({n \over 2})} on average:

@sicpnl[(define (adjoin-set x set)
          (cond [(null? set) (list x)]
                [(= x (car set)) set]
                [(< x (car set)) (cons x set)]
                [else (cons (car set)
                            (adjoin-set x (cdr set)))]))]

@sicpnl[(define (set-elements set) set)]

@sicp[(print-set (adjoin-set 3 '(1 2 4 5)))]
