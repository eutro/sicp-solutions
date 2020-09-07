#lang scribble/manual
@(require racket/sandbox
          scribble/example
          scribble-math/dollar
          "../sicp-eval.rkt")

@title[#:style (with-html5 manual-doc-style)]{Multiple Representations for Abstract Data}
@(use-katex)

@section{Exercise 2.73}

@sicp[#:hidden ;; copy from Chapter 3.3 for now
      (define (make-table)
        (let ((local-table (list '*table*)))
          (define (lookup key-1 key-2)
            (let ((subtable (assoc key-1 (cdr local-table))))
              (if subtable
                  (let ((record (assoc key-2 (cdr subtable))))
                    (if record
                        (cdr record)
                        false))
                  false)))
          (define (insert! key-1 key-2 value)
            (let ((subtable (assoc key-1 (cdr local-table))))
              (if subtable
                  (let ((record (assoc key-2 (cdr subtable))))
                    (if record
                        (set-cdr! record value)
                        (set-cdr! subtable
                                  (cons (cons key-2 value)
                                        (cdr subtable)))))
                  (set-cdr! local-table
                            (cons (list key-1
                                        (cons key-2 value))
                                  (cdr local-table)))))
            'ok)    
          (define (dispatch m)
            (cond ((eq? m 'lookup-proc) lookup)
                  ((eq? m 'insert-proc!) insert!)
                  (else (error "Unknown operation -- TABLE" m))))
          dispatch))

      (define operation-table (make-table))
      (define get (operation-table 'lookup-proc))
      (define put (operation-table 'insert-proc!))]

@sicp[#:label "Copied:"
      (define (deriv exp var)
        (cond ((number? exp) 0)
              ((variable? exp) (if (same-variable? exp var) 1 0))
              (else ((get 'deriv (operator exp)) (operands exp)
                                                 var))))
      (define (operator exp) (car exp))
      (define (operands exp) (cdr exp))]

@subsection{Exercise 2.73.a}

For the @tt{deriv} procedure, @tt{exp} can be a variable, a number or a list representing an operation.

@itemlist[@item{Constants (numbers and variables other than @tt{var}) have a derivative of @${0}.}
          @item{@tt{var} as a term has a derivative of @${1}.}
          @item{Operations have a @tt{deriv} procedure in the operation table.}]

Numbers and variables are represented differently to operations, so cannot be dispatched on in the same way.

@subsection{Exercise 2.73.b}

@sicpnl[(define (make-sum addend augend)
          (cond [(and (number? addend)
                      (number? augend))
                 (+ addend augend)]

                [(eqv? 0 addend) augend]

                [(eqv? 0 augend) addend]

                [else (list '+ addend augend)]))

        (define addend car)
        (define augend cadr)

        (define (install-sum-deriv-package)

          (define (sum-deriv operands var)
            (make-sum (deriv (addend operands) var)
                      (deriv (augend operands) var)))

          (put 'deriv '+ sum-deriv))
        
        (define (make-product multiplier multiplicand)
          (cond [(and (number? multiplier)
                      (number? multiplicand))
                 (* multiplier multiplicand)]

                [(or (eqv? 0 multiplier)
                     (eqv? 0 multiplicand)) 0]

                [(eqv? 1 multiplier)
                 multiplicand]

                [(eqv? 1 multiplicand)
                 multiplier]

                [else (list '* multiplier multiplicand)]))

        (define multiplier car)
        (define multiplicand cadr)
        
        (define (install-product-deriv-package)

          (define (product-deriv operands var)
            (make-sum (make-product (multiplier operands)
                                    (deriv (multiplicand operands) var))
                      (make-product (deriv (multiplier operands) var)
                                    (multiplicand operands))))

          (put 'deriv '* product-deriv))]

@sicp[(install-sum-deriv-package)
      (install-product-deriv-package)

      (deriv '(* x y) 'x)
      (print-list (deriv '(* (* x y) (+ x 3)) 'x))]

@subsection{Exercise 2.73.c}

@sicpnl[(define (make-exp base exponent)
          (cond [(or (eqv? 1 base)
                     (eqv? 0 exponent)) 1]

                [(eqv? 1 exponent) base]

                [(and (number? base)
                      (number? exponent))
                 (expt base exponent)]

                [else (list '** base exponent)]))

        (define base car)
        (define exponent cadr)

        (define (install-exp-deriv-package)

          (define (exp-deriv operands var)
            (if (same-variable? (base operands) var)
                     (make-product (exponent operands)
                                   (make-exponentiation (base operands)
                                                        (make-sum (exponent operands) -1)))
                     0))

          (put 'deriv '** exp-deriv))]

@sicp[(install-exp-deriv-package)

      (print-list (deriv '(+ (** x 2) (** x y)) 'x))]

@subsection{Exercise 2.73.d}

The deriv packages would also have to flip the operand and type arguments.
