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

@section{Exercise 2.74}

Let there be three divisions, @${A}, @${B} and @${C}, with their records
structured as such:

@sicpnl[(define records-a
          '(("Albert" ((address "1 A road")
                       (salary 1000)))
            ("Bertie" ((address "32 B road")
                       (salary 1300)))
            ("Cieran" ((address "17 C street")
                       (salary 1100)))))

        (define records-b
          '("Barry" ("4 B avenue"
                     1400)
                    ("Alfie" ("1 C lane"
                              2100)
                             ()
                             ())
                    ("Catherine" ("63 D close"
                                  1700)
                                 ()
                                 ())))

        (define records-c
          '("Andrew"
            (address
             "1001 L street"

             salary
             3000)

            "Beatrice"
            (address
             "20 E way"

             salary
             2700)

            "Chloe"
            (address
             "57 F path"

             salary
             1800)))]

These procedures are used by the divisions to access their data.

@sicpnl[(define (get-record-a file employee)
          (let ([record (assoc employee file)])
            (if record
                (cadr record)
                '())))

        (define (get-record-b file employee)
          (cond [(null? file) '()]

                [(string=? employee (car file))
                 (cadr file)]

                [(string<? employee (car file))
                 (get-record-b (caddr file) employee)]

                [else
                 (get-record-b (cadddr file) employee)]))

        (define (get-record-c file employee)
          (cond [(null? file) '()]
                [(string=? (car file) employee) (cadr file)]
                [else (get-record-c (cddr file) employee)]))]

@sicp[(print-list (get-record-a records-a "Albert"))
      (print-list (get-record-b records-b "Catherine"))
      (print-list (get-record-c records-c "Beatrice"))]

@subsection{Exercise 2.74.a}

@tt{get-record} can be defined as such:

@sicpnl[(define division car)
        (define (tag-division division file)
          (list division file))
        (define data cadr)

        (define (get-record file employee)
          (let ([optrecord ((get 'get-record (division file)) (data file) employee)])
            (if (null? optrecord)
                '()
                (tag-division (division file) optrecord))))]

The divisions need to add and install their packages:

@sicpnl[(define (install-division-a-get-record)
          (put 'records 'a (tag-division 'a records-a))
          (put 'get-record 'a get-record-a))
        (install-division-a-get-record)

        (define (install-division-b-get-record)
          (put 'records 'b (tag-division 'b records-b))
          (put 'get-record 'b get-record-b))
        (install-division-b-get-record)

        (define (install-division-c-get-record)
          (put 'records 'c (tag-division 'c records-c))
          (put 'get-record 'c get-record-c))
        (install-division-c-get-record)]

@sicp[(print-list (get-record (get 'records 'a) "Albert"))
      (print-list (get-record (get 'records 'b) "Catherine"))
      (print-list (get-record (get 'records 'c) "Beatrice"))]

@subsection{Exercise 2.74.b}

@sicpnl[(define (get-salary record)
          ((get 'get-salary (division record)) (data record)))

        (define (install-division-a-get-salary)
          (put 'get-salary 'a
               (lambda (record)
                 (cadr (assoc 'salary record)))))

        (define (install-division-b-get-salary)
          (put 'get-salary 'b cadr))

        (define (division-c-lookup key record)
          (define (loop k v . others)
            (if (eqv? key k)
                v
                (apply loop others)))
          (apply loop record))

        (define (install-division-c-get-salary)
          (put 'get-salary 'c (partial division-c-lookup 'salary)))]

@sicp[(install-division-a-get-salary)
      (install-division-b-get-salary)
      (install-division-c-get-salary)

      (get-salary (get-record (get 'records 'a) "Bertie"))
      (get-salary (get-record (get 'records 'b) "Alfie"))
      (get-salary (get-record (get 'records 'c) "Chloe"))]

@subsection{Exercise 2.74.c}

@sicpnl[(define (find-employee-record name files)
          (if (null? files)
              '()
              (let ([opt-record (get-record (car files)
                                            name)])
                (if (null? opt-record)
                    (find-employee-record name (cdr files))
                    opt-record))))]

@sicpnl[(define all-records
          (map (partial get 'records)
               '(a b c)))

        (print-list (find-employee-record "Andrew" all-records))
        (print-list (find-employee-record "Barry" all-records))
        (print-list (find-employee-record "Dave" all-records))]

@subsection{Exercise 2.74.d}

Each new division needs to:

@itemlist[@item{Add their records to @tt{all-records}.}
          @item{Add their @tt{get-record} and @tt{get-salary} procedures.}]

@section{Exercise 2.75}

@sicp[#:label "Copied:"
      (define (make-from-real-imag x y)
        (define (dispatch op)
          (cond ((eq? op 'real-part) x)
                ((eq? op 'imag-part) y)
                ((eq? op 'magnitude)
                 (sqrt (+ (square x) (square y))))
                ((eq? op 'angle) (atan y x))
                (else
                 (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
        dispatch)

      (define (apply-generic op arg) (arg op))]

@sicpnl[(define (make-from-mag-ang mag ang)
          (lambda (op)
            (cond [(eq? op 'magnitude) mag]
                  [(eq? op 'angle) ang]
                  [(eq? op 'real-part) (* (cos ang) mag)]
                  [(eq? op 'imag-part) (* (sin ang) mag)]
                  [else
                   (error "Unknown op -- MAKE-FROM-MAG-ANG" op)])))]

@sicpnl[(define imag-magnitude (partial apply-generic 'magnitude))
        (define imag-angle (partial apply-generic 'angle))
        (define imag-real-part (partial apply-generic 'real-part))
        (define imag-imag-part (partial apply-generic 'imag-part))

        (define (print-complex-cartesian imag)
          (display (rationalize (imag-real-part imag) 1/10))
          (display " + ")
          (display (rationalize (imag-imag-part imag) 1/10))
          (display "i")
          (newline))

        (define pi (* 2 (asin 1)))
        (define (print-angle radians)
          (display (inexact->exact (rationalize (/ radians pi)
                                                1/10)))
          (display " π"))

        (define (print-complex-polar imag)
          (print-angle (imag-angle imag))
          (display " : ")
          (display (rationalize (imag-magnitude imag) 1/10))
          (newline))]

@sicpnl[(print-complex-cartesian (make-from-mag-ang (sqrt 2)
                                                    (/ pi 4)))
        (print-complex-cartesian (make-from-real-imag 1.0 1.0))

        (print-complex-polar (make-from-real-imag 0.0 1.0))
        (print-complex-polar (make-from-mag-ang 1.0
                                                (/ pi 2)))]

@section{Exercise 2.76}

For explicit dispatch:

@itemlist[@item{To add a new type, new clauses need to be added to each procedure that operates on the type.}
          @item{To add a new operation, a single procedure needs to be written, and it should have a clause for each existing type.}]

For operation-table style:

@itemlist[@item{To add a new type, it needs to be installed in the table with all the operations implemented.}
          @item{To add a new operation, each type needs to install their own implementation to the table.}]

For message-passing style:

@itemlist[@item{To add a new type, it needs to implement all the operations in its constructor.}
          @item{To add a new operation, each type's constructor needs to add an extra operation.}]

@bold{For frequent addition of new types}, message-passing style or operation-table style are both good,
though message-passing style is slightly better, as installation in the operation-table may be tedious.

Explicit dispatch would be a poor choice, as it would require changes to existing operation procedures.

@bold{For frequent addition of new operations}, explicit dispatch and operation-table style are both good,
though explicit dispatch may be preferrable, as installation in the operation-table may be tedious.

Message-passing-style would be a poor choice, since it would require changes to each existing
type's constructor.
