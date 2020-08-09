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

@section{Exercise 2.3}

First, it is worth implementing some functions that act on
top of the abstraction, to identify what should be required
of it.

Area and perimeter are those which were requested...

@examples[#:eval sicp-evaluator #:label #f
          (define (rect-area rect)
            (* (rect-width rect)
               (rect-height rect)))
          (define (rect-perimeter rect)
            (* (+ (rect-height rect)
                  (rect-width rect))
               2))]

Thus, for that which is required, a simple pair
of width and height is sufficient.

@examples[#:eval sicp-evaluator #:label #f
          (define (make-rect width height)
            (if (or (< width 0)
                    (< height 0))
                (error "Rectangle with negative size")
                (cons width height)))
          (define rect-width car)
          (define rect-height cdr)]

@examples[#:eval sicp-evaluator
          (define rectangle (make-rect 4 8))
          (rect-area rectangle)
          (rect-perimeter rectangle)]

Another implementation could be a pair of points,
one representing the top-right corner, another the
bottom left.

Not only is this more expressive, in that it gives
the rectangle a position, it can also be normalized
at construction to always have positive width and
height.

@examples[#:eval sicp-evaluator #:label #f
          (define (make-rect corner opposite)
            (cond [(< (x-point opposite)
                      (x-point corner))

                   (make-rect (make-point (x-point opposite)
                                          (y-point corner))
                              (make-point (x-point corner)
                                          (y-point opposite)))]

                  [(< (y-point opposite)
                      (y-point corner))

                   (make-rect (make-point (x-point corner)
                                          (y-point opposite))
                              (make-point (x-point opposite)
                                          (y-point corner)))]

                  [else (cons corner opposite)]))

          (define rect-start car)
          (define rect-end cdr)

          (define (rect-width rect)
            (- (x-point (rect-end rect))
               (x-point (rect-start rect))))

          (define (rect-height rect)
            (- (y-point (rect-end rect))
               (y-point (rect-start rect))))]

@examples[#:eval sicp-evaluator
          (define rectangle (make-rect (make-point 2 3)
                                       (make-point 6 11)))
          (rect-area rectangle)
          (rect-perimeter rectangle)]

@section{Exercise 2.4}

@racket[(car (cons x y))] would evaluate as:

@racketblock[
  (car (cons x y))
  (car (lambda (m) (m x y)))
  ((lambda (m) (m x y)) (lambda (p q) p))
  ((lambda (p q) p) x y)
  x]

@tt{cdr} would look like:

@racketblock[(define (cdr z)
               (z (lambda (p q) q)))]

@section{Exercise 2.5}

Prefixing with @tt{n-} so the world doesn't break...

@examples[#:eval sicp-evaluator #:label #f
          (define (n-cons a b)
            (* (expt 2 a)
               (expt 3 b)))

          (define (log-base base)
            (let ([denom (log base)])
              (lambda (x)
                (/ (log x)
                   denom))))

          (define (pair-getter base other)
            (let ([log-func (log-base base)])
              (define (getter pair)
                (if (= (remainder pair other) 0)
                    (getter (/ pair other))
                    (log-func pair)))
              getter))

          (define n-car (pair-getter 2 3))

          (define n-cdr (pair-getter 3 2))]

@examples[#:eval sicp-evaluator
          (n-car (n-cons 5 10))
          (n-cdr (n-cons 8 12))]

@section{Exercise 2.6}

The Church numeral @tt{n} takes a function, and returns another that
repeats the original function @tt{n} times.

@examples[#:eval sicp-evaluator #:label #f
          (define one
            (lambda (f) f))

          (define two
            (lambda (f)
              (lambda (x)
                (f (f x)))))

          (define (add a b)
            (lambda (f)
              (lambda (x)
                ((a f) ((b f) x)))))]

@examples[#:eval sicp-evaluator
          (define (church->int church)
            ((church inc) 0))

          (church->int one)
          (church->int two)

          (church->int (add one
                            two))

          (church->int (add (add two
                                 one)
                            two))]

@examples[#:eval sicp-evaluator #:label "And multiplication, for fun."
          (define (multiply a b)
            (lambda (f)
              (a (b f))))]

@examples[#:eval sicp-evaluator
          (define zero
            (lambda (f)
              (lambda (x) x)))

          (define three (add one two))
          (define four (multiply two two))

          (church->int (multiply three
                                 two))

          (church->int (add (multiply three
                                      four)
                            one))

          (church->int (multiply zero
                                 three))]

@section{Exercise 2.7}

@examples[#:eval sicp-evaluator #:label "Copied..."
          (define (add-interval x y)
            (make-interval (+ (lower-bound x) (lower-bound y))
                           (+ (upper-bound x) (upper-bound y))))

          (define (mul-interval x y)
            (let ((p1 (* (lower-bound x) (lower-bound y)))
                  (p2 (* (lower-bound x) (upper-bound y)))
                  (p3 (* (upper-bound x) (lower-bound y)))
                  (p4 (* (upper-bound x) (upper-bound y))))
              (make-interval (min p1 p2 p3 p4)
                             (max p1 p2 p3 p4))))

          (define (div-interval x y)
            (mul-interval x
                          (make-interval (/ 1.0 (upper-bound y))
                                         (/ 1.0 (lower-bound y)))))

          (define (make-interval a b) (cons a b))

          (define (make-center-width c w)
            (make-interval (- c w) (+ c w)))
          (define (center i)
            (/ (+ (lower-bound i) (upper-bound i)) 2))
          (define (width i)
            (/ (- (upper-bound i) (lower-bound i)) 2))]

@examples[#:eval sicp-evaluator #:label #f
          (define upper-bound car)
          (define lower-bound cdr)]

@section{Exercise 2.8}

The minimum value the subtraction could be is the
difference of the respective lower and upper bounds
and the maximum value it could be is the difference
of the upper and lower bounds respectively:

@examples[#:eval sicp-evaluator #:label #f
          (define (sub-interval x y)
            (make-interval (- (lower-bound x) (upper-bound y))
                           (- (upper-bound x) (lower-bound y))))]

@examples[#:eval sicp-evaluator
          (define (print-interval interval)
            (display (center interval))
            (display "±")
            (display (width interval))
            (newline))

          (define 15+-5 (make-center-width 15 5))
          (define 2+-1 (make-center-width 2 1))

          (print-interval (sub-interval 15+-5
                                        2+-1))]

@section{Exercise 2.9}

For the addition and subtraction of any two intervals a and b,
the resulting interval will simply have a width which is the
sum of a and b's respective widths.

Where
@itemlist[@item{@${[x, y]} is the interval between @${x} and @${y}}
          @item{@${C_a} is the center of the interval @${a}}
          @item{@${W_a} is the width of the interval @${a}}
          @item{@${U_a} is the upper bound of the interval @${a}}
          @item{@${L_a} is the lower bound of the interval @${a}}]

Addition can be written as such:
@$${a + b = c}
@$${[L_a, U_a] + [L_b, U_b] = [L_a + L_b, U_a + U_b]}
@$${= [C_a - W_a, C_a + W_a] + [C_b - W_b, C_b + W_b]}
@$${= [(C_a - W_a) + (C_b - W_b), (C_a + W_a) + (C_b + W_b)]}
@$${= [(C_a + C_b) - (W_a + W_b), (C_a + C_b) + (W_a + W_b)] = c}

Calculating its width:
@$${W_c = {U_c - L_c \over 2}}
@$${= {((C_a + C_b) + (W_a + W_b)) - ((C_a + C_b) - (W_a + W_b)) \over 2}}
@$${= {2(W_a + W_b) \over 2}}
@$${= W_a + W_b}

Similarly for subtraction:
@$${a - b = c}
@$${[L_a, U_a] - [L_b, U_b] = [L_a - U_b, U_a - L_b]}
@$${= [C_a - W_a, C_a + W_a] - [C_b - W_b, C_b + W_b]}
@$${= [(C_a - W_a) - (C_b + W_b), (C_a + W_a) - (C_b - W_b)]}
@$${= [(C_a - C_b) - (W_a + W_b), (C_a - C_b) + (W_a + W_b)] = c}

Calculating its width:
@$${W_c = {U_c - L_c \over 2}}
@$${= {((C_a - C_b) + (W_a + W_b)) - ((C_a - C_b) - (W_a + W_b)) \over 2}}
@$${= {2(W_a + W_b) \over 2}}
@$${= W_a + W_b}
