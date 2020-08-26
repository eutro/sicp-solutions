#lang scribble/manual
@(require racket/sandbox
          scribble/example
          scribble-math/dollar)
@(define sicp-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50]
                  [sandbox-propagate-exceptions #f])
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
          (define lower-bound car)
          (define upper-bound cdr)]

@section{Exercise 2.8}

The minimum value the subtraction could be is the
difference of the respective lower and upper bounds
and the maximum value it could be is the difference
of the upper and lower bounds respectively:

@examples[#:eval sicp-evaluator #:label #f
          (define (sub-interval x y)
            (make-interval (- (lower-bound x) (upper-bound y))
                           (- (upper-bound x) (lower-bound y))))]

@examples[#:eval sicp-evaluator #:label "Some interval printing..."
          (define (print-int interval)
            (display "[")
            (display (lower-bound interval))
            (display ",")
            (display (upper-bound interval))
            (display "]")
            (newline))

          (define (print-int-cw interval)
            (display (center interval))
            (display "±")
            (display (width interval))
            (newline))

          (define (print-int-cpct interval)
            (display (center interval))
            (display "±")
            (display (percent interval))
            (display "%")
            (newline))]

@examples[#:eval sicp-evaluator
          (define 15+-5 (make-center-width 15 5))
          (define 2+-1 (make-center-width 2 1))

          (print-int-cw (sub-interval 15+-5
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

For multiplication and division, in the following examples,
@tt{int-a} and @tt{int-b} have the same width. When multiplied
(or divided) by @tt{int-c}, the results have different widths.

@examples[#:eval sicp-evaluator #:label #f
          (define int-a (make-center-width 3 1))
          (define int-b (make-center-width 5 1))
          (define int-c (make-interval 1 2))

          (print-int-cw int-a)
          (print-int-cw int-b)
          (print-int-cw int-c)

          (print-int-cw (mul-interval int-a int-c))
          (print-int-cw (mul-interval int-b int-c))

          (print-int-cw (div-interval int-a int-c))
          (print-int-cw (div-interval int-b int-c))]

Thus, the resulting width of interval division or multiplication
is dependent not solely on width.

@section{Exercise 2.10}

@examples[#:eval sicp-evaluator #:label #f
          (define (spans-zero? interval)
            (and (<= (lower-bound interval) 0)
                 (>= (upper-bound interval) 0)))

          (define (div-interval x y)
            (if (spans-zero? y)
                (error "Interval division spanning zero")
                (mul-interval x
                              (make-interval (/ 1.0 (upper-bound y))
                                             (/ 1.0 (lower-bound y))))))]

@examples[#:eval sicp-evaluator
          (div-interval (make-interval 1 1)
                        (make-interval -1 1))]

@section{Exercise 2.11}

@examples[#:eval sicp-evaluator #:label "The previous implementation, to compare:"
          (define (mul-intervals l-x u-x l-y u-y)
            (print-int (mul-interval (make-interval l-x u-x)
                                     (make-interval l-y u-y))))

          (mul-intervals 1 2
                         3 4)

          (mul-intervals  1 2
                         -3 4)

          (mul-intervals  1  2
                         -4 -3)

          (mul-intervals -1 2
                          3 4)

          (mul-intervals -1 2
                         -3 4)

          (mul-intervals -1  2
                         -4 -3)

          (mul-intervals -2 -1
                          3  4)

          (mul-intervals -2 -1
                         -3  4)

          (mul-intervals -2 -1
                         -4 -3)]

@examples[#:eval sicp-evaluator #:label #f
          (define (pos? n)
            (>= n 0))

          (define (mul-interval x y)
            (let ([l- lower-bound]
                  [u- upper-bound]
                  [mi (lambda (a b c d)
                        (make-interval (* (a x)
                                          (b y))
                                       (* (c x)
                                          (d y))))])

              (case (+ (if (pos? (l- x)) 8 0)
                       (if (pos? (u- x)) 4 0)
                       (if (pos? (l- y)) 2 0)
                       (if (pos? (u- y)) 1 0))
                [0  (mi u- u- l- l-)]
                [1  (mi l- u- l- l-)]
                [3  (mi l- u- u- l-)]
                [4  (mi u- l- l- l-)]
                [5  (make-interval (min (* (l- x)
                                           (u- y))
                                        (* (u- x)
                                           (l- y)))
                                   (max (* (u- x)
                                           (u- y))
                                        (* (l- x)
                                           (l- y))))]
                [7  (mi l- u- u- u-)]
                [12 (mi u- l- l- u-)]
                [13 (mi u- l- u- u-)]
                [15 (mi l- l- u- u-)])))]

@examples[#:eval sicp-evaluator
          (mul-intervals 1 2
                         3 4)

          (mul-intervals  1 2
                         -3 4)

          (mul-intervals  1  2
                         -4 -3)

          (mul-intervals -1 2
                          3 4)

          (mul-intervals -1 2
                         -3 4)

          (mul-intervals -1  2
                         -4 -3)

          (mul-intervals -2 -1
                          3  4)

          (mul-intervals -2 -1
                         -3  4)

          (mul-intervals -2 -1
                         -4 -3)]

@section{Exercise 2.12}

@examples[#:eval sicp-evaluator #:label #f
          (define (make-center-percent c pct)
            (make-center-width c
                               (* (/ pct
                                     100)
                                  c)))

          (define (percent interval)
            (* (/ (width interval)
                  (center interval))
               100))]

@examples[#:eval sicp-evaluator
          (print-int-cpct (make-center-percent 1000 25))
          (print-int-cw (make-center-percent 3000 5))]

@section{Exercise 2.13}

With all bounds positive, and with the definitions as in
@secref{Exercise_2_9}, multiplication can be written as:
@$${ab = c}
@$${[L_a, U_a][L_b, U_b] =}
@$${[L_aL_b, U_aU_b] = c}

Where @${T_a = {W_a \over C_a}} (percentage tolerance divided by @${100}):

@$${L_a = C_a(1 - T_a)}
@$${U_a = C_a(1 + T_a)}

Thus:

@$${c = [C_aC_b(1 - T_a)(1 - T_b), C_aC_b(1 + T_a)(1 + T_b)]}

The center:

@$${C_c = {C_aC_b(1 - T_a)(1 - T_b) + C_aC_b(1 + T_a)(1 + T_b) \over 2}}
@$${= {C_aC_b[(1 - T_a)(1 - T_b) + (1 + T_a)(1 + T_b)] \over 2}}
@$${= {2C_aC_b(1 + T_aT_b) \over 2}}
@$${= C_aC_b(1 + T_aT_b)}

And the width:

@$${W_c = {C_aC_b(1 + T_a)(1 + T_b) - C_aC_b(1 - T_a)(1 - T_b) \over 2}}
@$${= {C_aC_b[(1 + T_a)(1 + T_b) - (1 - T_a)(1 - T_b)] \over 2}}
@$${= {2C_aC_b(T_a + T_b) \over 2}}
@$${= C_aC_b(T_a + T_b)}

Finally:

@$${T_c = {W_c \over C_c}}
@$${= {C_aC_b(T_a + T_b) \over C_aC_b(1 + T_aT_b)}}
@$${= {T_a + T_b \over 1 + T_aT_b}}

Thus, with small percentage tolerance:

@$${T_c \approx T_a + T_b}

@section{Exercise 2.14}

@examples[#:eval sicp-evaluator #:label "Copying..."
          (define (par1 r1 r2)
            (div-interval (mul-interval r1 r2)
                          (add-interval r1 r2)))
          (define (par2 r1 r2)
            (let ((one (make-interval 1 1)))
              (div-interval one
                            (add-interval (div-interval one r1)
                                          (div-interval one r2)))))]

@examples[#:eval sicp-evaluator
          (define r_A (make-center-percent 240 8))
          (define r_B (make-center-percent 960 10))

          (print-int-cpct r_A)
          (print-int-cpct r_B)

          (print-int-cpct (par1 r_A r_B))
          (print-int-cpct (par2 r_A r_B))]

Clearly, the given examples demonstrate different results.

@examples[#:eval sicp-evaluator #:label "Here are some more examples:"
          (print-int-cpct (div-interval r_A r_A))

          (print-int-cpct (mul-interval r_B
                                        (div-interval r_A
                                                      r_B)))

          (print-int-cw (sub-interval r_A r_A))

          (print-int-cpct (add-interval r_B
                                        (sub-interval r_A
                                                      r_B)))]

The penultimate one isn't percent width, since percentage of zero makes no sense.

Notably, with intervals @${a} and @${b},

@$${{a \over a} \neq [1, 1]}
@$${a - a \neq [0, 0]}
@$${b{a \over b} \neq a}
@$${a - b + b \neq a}

@section{Exercise 2.15}

Each time an uncertain interval appears, it introduces some uncertainty. Uncertainty
increases with each operation of two uncertain values. Thus, reducing the number
of uncertain variables, by avoiding repetitions, reduces the uncertainty of the calculation
overall.

@section{Exercise 2.16}

As shown in @secref{Exercise_2_14}, otherwise algebraically equivalent expressions, such as

@$${{a \over a} = 1}
@$${a - a = 0}
@$${b{a \over b} = a}
@$${a - b + b = a}

don't hold for intervals with this program.

The program treats any two intervals as distinct. Thus, though an interval @${a} can only
have a single value, and algebraic equivalences should hold, the package does not take this
into account, and interprets any two occurences of the same variable as uncertain values
in their own right.

Devising a package without this shortcoming may be possible, by storing performed
operations instead of calculating them on the spot, then operating on those in some way.
However, a system like that would be significantly more complicated, and likely far less
performant.

And to answer the question, I personally would be unable to do this.

@section{Exercise 2.17}

@examples[#:eval sicp-evaluator #:label "List printing:"
          (define (print-list l)
            (display "(")
            (define (iter l)
              (begin (if (pair? (car l))
                         (print-list (car l))
                         (display (car l)))
                     (if (null? (cdr l))
                         (display ")")
                         (begin (display " ")
                                (iter (cdr l))))))
            (if (null? l)
                (display ")")
                (iter l)))]

@examples[#:eval sicp-evaluator
          (print-list nil)
          (print-list (list 1 2 3 4))]

@examples[#:eval sicp-evaluator #:label #f
          (define (last-pair l)
            (if (null? (cdr l))
                l
                (last-pair (cdr l))))]

@examples[#:eval sicp-evaluator
          (print-list (last-pair (list 23 72 149 34)))]

@section{Exercise 2.18}

@examples[#:eval sicp-evaluator #:label #f
          (define (reversed l)
            (define (iter old-list
                          new-list)
              (if (null? old-list)
                  new-list
                  (iter (cdr old-list)
                        (cons (car old-list)
                              new-list))))
            (iter l nil))]

@examples[#:eval sicp-evaluator
          (print-list (reversed (list 1 4 9 16 25)))]

@section{Exercise 2.19}

@examples[#:eval sicp-evaluator #:label "Defining coin values..."
          (define us-coins (list 50 25 10 5 1))
          (define uk-coins (list 100 50 20 10 5 2 1 0.5))]

@examples[#:eval sicp-evaluator #:label #f
          (define (cc amount coin-values)
            (cond ((= amount 0) 1)
                  ((or (< amount 0) (no-more? coin-values)) 0)
                  (else
                   (+ (cc amount
                          (except-first-denomination coin-values))
                      (cc (- amount
                             (first-denomination coin-values))
                          coin-values)))))

          (define first-denomination car)
          (define except-first-denomination cdr)
          (define no-more? null?)]

@examples[#:eval sicp-evaluator
          (cc 100 us-coins)
          (cc 100 uk-coins)]

The order of the coins does not matter, since all possible branches will be explored anyway.

@examples[#:eval sicp-evaluator #:label #f
          (cc 100 (reverse us-coins))
          (cc 100 (list 25 10 1 5 50))]

@section{Exercise 2.20}

@examples[#:eval sicp-evaluator #:label #f
          (define (filter accept? l)
            (define (iter old-list new-list)
              (if (null? old-list)
                  new-list
                  (iter (cdr old-list)
                        (if (accept? (car old-list))
                            (cons (car old-list) new-list)
                            new-list))))
            (reverse (iter l nil)))

          (define (same-parity first . rest)
            (filter (if (even? first)
                        even?
                        (lambda (x) (not (even? x))))
                    (cons first rest)))]

@examples[#:eval sicp-evaluator
          (print-list (same-parity 1 2 3 4 5 6 7))
          (print-list (same-parity 2 3 4 5 6 7))]

@section{Exercise 2.21}

@examples[#:eval sicp-evaluator #:label #f
          (define (square x) (* x x))

          (define (square-list items)
            (if (null? items)
                nil
                (cons (square (car items))
                      (square-list (cdr items)))))
          (print-list (square-list (list 1 2 3 4)))

          (define (square-list items)
            (map square items))
          (print-list (square-list (list 1 2 3 4)))]

@section{Exercise 2.22}

@racketblock[
  (define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))]

Using @tt{cons}, one prepends a value to an existing list.

Thus, iterating over a list, the elements of the original list are
prepended sequentially to the new list, making the new list a reverse
of the old one.

@racketblock[
  (define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))]

A list is a pair, whose first value is the first value of the list,
and whose second value is the rest of the list.

Here, Louis is making a pair whose first element is the rest of the
new list, and the second element is the squared number.

Thus, by switching the arguments of @tt{cons}, what Louis has created
is not a list at all.

@section{Exercise 2.23}

@examples[#:eval sicp-evaluator #:label #f
          (define (for-each proc l)
            (if (null? l)
                true
                (begin (proc (car l))
                       (for-each proc
                                 (cdr l)))))]

@examples[#:eval sicp-evaluator
          (for-each (lambda (x) (newline) (display x))
                    (list 57 321 88))]

@section{Exercise 2.24}

@examples[#:eval sicp-evaluator #:label #f
          (print-list (list 1 (list 2 (list 3 4))))]

@image["chapters/two/asymptote-images/bf7f8a599ac84cc365ca6bddef1083f9"
       #:suffixes (list ".svg"
                        ".png"
                        ".pdf")]

@image["chapters/two/asymptote-images/40852fa848efabdd243c95d1d78ff84e"
       #:scale 2
       #:suffixes (list ".svg"
                        ".png"
                        ".pdf")]

@section{Exercise 2.25}

@examples[#:eval sicp-evaluator #:label #f
          (car (cdaddr (list 1 3 (list 5 7) 9)))
          (caar (list (list 7)))
          (cadadr (cadadr (cadadr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))]

@section{Exercise 2.26}

@examples[#:eval sicp-evaluator #:label #f
          (define x (list 1 2 3))
          (define y (list 4 5 6))

          (print-list (append x y))
          (print-list (cons x y))
          (print-list (list x y))]

@section{Exercise 2.27}

@examples[#:eval sicp-evaluator #:label #f
          (define (deep-reverse l)
            (map (lambda (el)
                   (if (list? el)
                       (deep-reverse el)
                       el))
                 (reverse l)))]

@examples[#:eval sicp-evaluator
          (define x (list (list 1 2) (list 3 4)))

          (print-list x)
          (print-list (reverse x))
          (print-list (deep-reverse x))]

@section{Exercise 2.28}

@examples[#:eval sicp-evaluator #:label #f
          (define (reduce reducer
                          base
                          l)
            (define (iter result remaining)
              (if (null? remaining)
                  result
                  (iter (reducer result
                                 (car remaining))
                        (cdr remaining))))
            (iter base l))

          (define (fringe l)
            (reduce (lambda (result
                             el)
                      (if (list? el)
                          (append result (fringe el))
                          (append result (list el)))) nil l))]

@examples[#:eval sicp-evaluator
          (print-list (fringe x))
          (print-list (fringe (list x x)))]

@section{Exercise 2.29}

@examples[#:eval sicp-evaluator #:label "Copied:"
          (define (make-mobile left right)
            (list left right))

          (define (make-branch length structure)
            (list length structure))]

@subsection{Exercise 2.29.a}

@examples[#:eval sicp-evaluator #:label #f
          (define left-branch car)
          (define right-branch cadr)]

@examples[#:eval sicp-evaluator
          (define test-mobile
            (make-mobile (make-branch 3 15)
                         (make-branch 1 (make-mobile (make-branch 2 15)
                                                     (make-branch 1 30)))))

          (print-list (left-branch test-mobile))
          (print-list (right-branch test-mobile))]

@subsection{Exercise 2.29.b}

@examples[#:eval sicp-evaluator #:label "More selectors:"
          (define branch-length car)
          (define branch-struct cadr)
          (define mobile? pair?)]

@examples[#:eval sicp-evaluator #:label #f
          (define (branch-weight branch)
            (if (mobile? (branch-struct branch))
                (total-weight (branch-struct branch))
                (branch-struct branch)))

          (define (total-weight mobile)
            (+ (branch-weight (left-branch mobile))
               (branch-weight (right-branch mobile))))]

@examples[#:eval sicp-evaluator
          (total-weight test-mobile)]

@subsection{Exercise 2.29.c}

@examples[#:eval sicp-evaluator #:label "This implementation is inefficient due to repeated recursive weight checks..."
          (define (branch-torque branch)
            (* (branch-length branch)
               (branch-weight branch)))

          (define (branch-balanced? branch)
            (or (not (mobile? (branch-struct branch)))
                (balanced? (branch-struct branch))))

          (define (balanced? mobile)
            (and (branch-balanced? (left-branch mobile))
                 (branch-balanced? (right-branch mobile))
                 (= (branch-torque (left-branch mobile))
                    (branch-torque (right-branch mobile)))))]

@examples[#:eval sicp-evaluator
          (balanced? test-mobile)
          (define (balanced-mobile)
            (make-mobile (make-branch 1 (make-mobile (make-branch 4 8)
                                                              (make-branch 2 16)))
                                  (make-branch 2 (make-mobile (make-branch 1 (make-mobile (make-branch 1 6)
                                                                                          (make-branch 3 2)))
                                                              (make-branch 2 4)))))
          (balanced? (balanced-mobile))

          (define (unbalanced-mobile)
            (make-mobile (make-branch 1 (make-mobile (make-branch 2 8)
                                                     (make-branch 4 16)))
                         (make-branch 2 (make-mobile (make-branch 1 (make-mobile (make-branch 1 6)
                                                                                 (make-branch 3 2)))
                                                     (make-branch 2 4)))))
          (balanced? (unbalanced-mobile))]

@subsection{Exercise 2.29.d}

@examples[#:eval sicp-evaluator #:label #f
          (define (make-mobile left right)
            (cons left right))
          (define (make-branch length structure)
            (cons length structure))]

@examples[#:eval sicp-evaluator #:label "Only a couple selectors have to be changed:"
          (define right-branch cdr)
          (define branch-struct cdr)]

@examples[#:eval sicp-evaluator
          (balanced? (balanced-mobile))
          (balanced? (unbalanced-mobile))]

@section{Exercise 2.30}

@examples[#:eval sicp-evaluator #:label "Directly:"
          (define (square-tree tree)
            (if (null? tree)
                nil
                (cons (let ([node (car tree)])
                        (if (pair? node)
                            (square-tree node)
                            (square node)))
                      (square-tree (cdr tree)))))]

@examples[#:eval sicp-evaluator
          (define test-tree
            (list 1
                  (list 2 (list 3 4) 5)
                  (list 6 7)))

          (print-list test-tree)
          (print-list (square-tree test-tree))]

@examples[#:eval sicp-evaluator #:label "In terms of map:"
          (define (square-tree tree)
            (reduce (lambda (a b) (append a (list b)))
                    nil
                    (map (lambda (node)
                           (if (pair? node)
                               (square-tree node)
                               (square node)))
                         tree)))]

@examples[#:eval sicp-evaluator
          (print-list (square-tree test-tree))]

@section{Exercise 2.31}

@examples[#:eval sicp-evaluator #:label #f
          (define (tree-map proc tree)
            (define (recur tree)
              (reduce (lambda (a b)
                        (append a (list b)))
                      nil
                      (map (lambda (node)
                             (if (pair? node)
                                 (recur node)
                                 (proc node)))
                           tree)))
            (recur tree))

          (define (square-tree tree) (tree-map square tree))]

@examples[#:eval sicp-evaluator
          (print-list (square-tree test-tree))]

@section{Exercise 2.32}

@examples[#:eval sicp-evaluator #:label #f
          (define (partial proc a)
            (lambda (b) (proc a b)))

          (define (subsets s)
            (if (null? s)
                (list nil)
                (let ((rest (subsets (cdr s))))
                  (append rest (map (partial cons (car s)) rest)))))]

@examples[#:eval sicp-evaluator
          (print-list (subsets (list 1 2 3)))]

The power set (set of all subsets) of a finite set @${S}, @${\mathcal{P}(S)}, can be defined recursively as:

For all @${x} in @${S}
and all @${T} that are a subset of @${S} without @${x},

@$${\{\text{subsets of }S\} =}
@$${\{t\text{ union }\{x\}\text{ such that }t\text{ is a subset of }T\}\text{ union }\{\text{subsets of }T\}}

Or formally:

@$${\forall x \in S \wedge T \subset S \wedge x \notin T,}
@$${\mathcal{P}(S) = \{t \cup \{x\} : t \subseteq T\} \cup \mathcal{P}(T)}

With the base case that:

@$${\mathcal{P}(\emptyset) = \{\emptyset\}}

@section{Exercise 2.33}

@examples[#:eval sicp-evaluator #:label "Redefining according to the book:"
          (define (filter predicate sequence)
            (cond ((null? sequence) nil)
                  ((predicate (car sequence))
                   (cons (car sequence)
                         (filter predicate (cdr sequence))))
                  (else (filter predicate (cdr sequence)))))

          (define (accumulate op initial sequence)
            (if (null? sequence)
                initial
                (op (car sequence)
                    (accumulate op initial (cdr sequence)))))]

@examples[#:eval sicp-evaluator #:label #f
          (define (map p sequence)
            (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
          (define (appended seq1 seq2)
            (accumulate cons seq2 seq1))
          (define (length-of sequence)
            (accumulate (lambda (x y) (inc y)) 0 sequence))]

@examples[#:eval sicp-evaluator
          (print-list (map square (list 1 2 3 4 5)))
          (print-list (appended (list 1 2 3 4) (list 5 6 7 8)))
          (length-of (list 1 2 3 4 5 6 7 8))]

@section{Exercise 2.34}

@examples[#:eval sicp-evaluator #:label #f
          (define (horner-eval x coefficient-sequence)
            (accumulate (lambda (this-coeff higher-terms)
                          (+ (* higher-terms x)
                             this-coeff))
                        0
                        coefficient-sequence))]

@examples[#:eval sicp-evaluator
          (horner-eval 2 (list 1 3 0 5 0 1))]

@section{Exercise 2.35}

@examples[#:eval sicp-evaluator #:label #f
          (define (count-leaves t)
            (accumulate (lambda (node-count total)
                          (+ node-count total))
                        0
                        (map (lambda (node)
                               (if (pair? node)
                                   (count-leaves node)
                                   1))
                             t)))]

@examples[#:eval sicp-evaluator
          (print-list x)
          (count-leaves (list x x))]
