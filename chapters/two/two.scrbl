#lang scribble/manual
@(require racket/sandbox
          scribble/example
          scribble-math/dollar
          "../sicp-eval.rkt")

@title[#:style (with-html5 manual-doc-style)]{Hierarchical Data and the Closure Property}
@(use-katex)

@section{Exercise 2.17}

@sicp[#:label "List printing:"
      (define (print-el el)
        (cond [(list? el) (print-list el)]
              [(pair? el) (print-pair el)]
              [else (write el)]))
      (define (print-list l)
        (display "(")
        (define (iter l)
          (begin (print-el (car l))
                 (if (null? (cdr l))
                     (display ")")
                     (begin (display " ")
                            (iter (cdr l))))))
        (if (null? l)
            (display ")")
            (iter l)))
      (define (print-pair p)
        (display "(")
        (print-el (car p))
        (display " . ")
        (print-el (cdr p))
        (display ")"))]

@sicp[
 (print-list nil)
 (print-list (list 1 2 3 4))
 (print-pair (cons 1 2))
 (print-list (list 1 2 (cons 3 4)))]

@sicpnl[
 (define (last-pair l)
   (if (null? (cdr l))
       l
       (last-pair (cdr l))))]

@sicp[
 (print-list (last-pair (list 23 72 149 34)))]

@section{Exercise 2.18}

@sicpnl[
 (define (reversed l)
   (define (iter old-list
                 new-list)
     (if (null? old-list)
         new-list
         (iter (cdr old-list)
               (cons (car old-list)
                     new-list))))
   (iter l nil))]

@sicp[
 (print-list (reversed (list 1 4 9 16 25)))]

@section{Exercise 2.19}

@sicp[#:label "Defining coin values..."
      (define us-coins (list 50 25 10 5 1))
      (define uk-coins (list 100 50 20 10 5 2 1 0.5))]

@sicpnl[
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

@sicp[
 (cc 100 us-coins)
 (cc 100 uk-coins)]

The order of the coins does not matter, since all possible branches will be explored anyway.

@sicpnl[
 (cc 100 (reverse us-coins))
 (cc 100 (list 25 10 1 5 50))]

@section{Exercise 2.20}

@sicpnl[
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

@sicp[
 (print-list (same-parity 1 2 3 4 5 6 7))
 (print-list (same-parity 2 3 4 5 6 7))]

@section{Exercise 2.21}

@sicpnl[
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

@sicpnl[
 (define (for-each proc l)
   (if (null? l)
       true
       (begin (proc (car l))
              (for-each proc
                        (cdr l)))))]

@sicp[
 (for-each (lambda (x) (newline) (display x))
           (list 57 321 88))]

@section{Exercise 2.24}

@sicpnl[
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

@sicpnl[
 (car (cdaddr (list 1 3 (list 5 7) 9)))
 (caar (list (list 7)))
 (cadadr (cadadr (cadadr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))]

@section{Exercise 2.26}

@sicpnl[
 (define x (list 1 2 3))
 (define y (list 4 5 6))

 (print-list (append x y))
 (print-list (cons x y))
 (print-list (list x y))]

@section{Exercise 2.27}

@sicpnl[
 (define (deep-reverse l)
   (map (lambda (el)
          (if (list? el)
              (deep-reverse el)
              el))
        (reverse l)))]

@sicp[
 (define x (list (list 1 2) (list 3 4)))

 (print-list x)
 (print-list (reverse x))
 (print-list (deep-reverse x))]

@section{Exercise 2.28}

@sicpnl[
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

@sicp[
 (print-list (fringe x))
 (print-list (fringe (list x x)))]

@section{Exercise 2.29}

@sicp[#:label "Copied:"
      (define (make-mobile left right)
        (list left right))

      (define (make-branch length structure)
        (list length structure))]

@subsection{Exercise 2.29.a}

@sicpnl[
 (define left-branch car)
 (define right-branch cadr)]

@sicp[
 (define test-mobile
   (make-mobile (make-branch 3 15)
                (make-branch 1 (make-mobile (make-branch 2 15)
                                            (make-branch 1 30)))))

 (print-list (left-branch test-mobile))
 (print-list (right-branch test-mobile))]

@subsection{Exercise 2.29.b}

@sicp[#:label "More selectors:"
      (define branch-length car)
      (define branch-struct cadr)
      (define mobile? pair?)]

@sicpnl[
 (define (branch-weight branch)
   (if (mobile? (branch-struct branch))
       (total-weight (branch-struct branch))
       (branch-struct branch)))

 (define (total-weight mobile)
   (+ (branch-weight (left-branch mobile))
      (branch-weight (right-branch mobile))))]

@sicp[
 (total-weight test-mobile)]

@subsection{Exercise 2.29.c}

@sicp[#:label "This implementation is inefficient due to repeated recursive weight checks..."
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

@sicp[
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

@sicpnl[
 (define (make-mobile left right)
   (cons left right))
 (define (make-branch length structure)
   (cons length structure))]

@sicp[#:label "Only a couple selectors have to be changed:"
      (define right-branch cdr)
      (define branch-struct cdr)]

@sicp[
 (balanced? (balanced-mobile))
 (balanced? (unbalanced-mobile))]

@section{Exercise 2.30}

@sicp[#:label "Directly:"
      (define (square-tree tree)
        (if (null? tree)
            nil
            (cons (let ([node (car tree)])
                    (if (pair? node)
                        (square-tree node)
                        (square node)))
                  (square-tree (cdr tree)))))]

@sicp[
 (define test-tree
   (list 1
         (list 2 (list 3 4) 5)
         (list 6 7)))

 (print-list test-tree)
 (print-list (square-tree test-tree))]

@sicp[#:label "In terms of map:"
      (define (square-tree tree)
        (reduce (lambda (a b) (append a (list b)))
                nil
                (map (lambda (node)
                       (if (pair? node)
                           (square-tree node)
                           (square node)))
                     tree)))]

@sicp[
 (print-list (square-tree test-tree))]

@section{Exercise 2.31}

@sicpnl[
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

@sicp[
 (print-list (square-tree test-tree))]

@section{Exercise 2.32}

@sicpnl[
 (define (partial proc . partial-args)
   (lambda args
     (apply proc (append partial-args args))))

 (define (subsets s)
   (if (null? s)
       (list nil)
       (let ((rest (subsets (cdr s))))
         (append rest (map (partial cons (car s)) rest)))))]

@sicp[
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

@sicp[#:label "Redefining according to the book:"
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

@sicpnl[
 (define (mapped p sequence)
   (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
 (define (appended seq1 seq2)
   (accumulate cons seq2 seq1))
 (define (length-of sequence)
   (accumulate (lambda (x y) (inc y)) 0 sequence))]

@sicp[
 (print-list (mapped square (list 1 2 3 4 5)))
 (print-list (appended (list 1 2 3 4) (list 5 6 7 8)))
 (length-of (list 1 2 3 4 5 6 7 8))]

@section{Exercise 2.34}

@sicpnl[
 (define (horner-eval x coefficient-sequence)
   (accumulate (lambda (this-coeff higher-terms)
                 (+ (* higher-terms x)
                    this-coeff))
               0
               coefficient-sequence))]

@sicp[
 (horner-eval 2 (list 1 3 0 5 0 1))]

@section{Exercise 2.35}

@sicpnl[
 (define (count-leaves t)
   (accumulate (lambda (node-count total)
                 (+ node-count total))
               0
               (map (lambda (node)
                      (if (pair? node)
                          (count-leaves node)
                          1))
                    t)))]

@sicp[
 (print-list x)
 (count-leaves (list x x))]

@section{Exercise 2.36}

@sicpnl[
 (define (accumulate-n op init seqs)
   (if (null? (car seqs))
       nil
       (cons (accumulate op init (map car seqs))
             (accumulate-n op init (map cdr seqs)))))]

@sicp[
 (define s (list (list  1  2  3)
                 (list  4  5  6)
                 (list  7  8  9)
                 (list 10 11 12)))
 (print-list (accumulate-n + 0 s))]

@section{Exercise 2.37}

@sicp[#:label "Copied:"
      (define (dot-product v w)
        (accumulate + 0 (map * v w)))]

@sicp[#:hidden #t
 (define (hyperop base exponent op identity)
   (define (hyperop-iter state base exponent)
     (cond ((= exponent 0) state)
           ((even? exponent) (hyperop-iter state
                                           (op base base)
                                           (/ exponent 2)))
           (else (hyperop-iter (op state base)
                               base
                               (dec exponent)))))
   (define (even? n)
     (= (remainder n 2) 0))
   (hyperop-iter identity base exponent))
 (define (compose f g)
   (lambda (x)
     (f (g x))))
 (define (repeated f times)
   (hyperop f times compose (lambda (x) x)))]

@sicp[#:label "Matrix utils:"
      (define (padded-num num count char)
        (string-append (make-string (- count (num-length num))
                                    char)
                       (number->string num)))
      (define (num-length number)
        (string-length (number->string number)))

      (define (print-matrix mat)
        (define el-len
          (apply max
                 (map (lambda (line)
                        (apply max
                               (map num-length
                                    line)))
                      mat)))
        (for-each (lambda (line)
                    (for-each (lambda (el)
                                (display (padded-num el el-len #\space))
                                (display " "))
                              line)
                    (newline))
                  mat))

      (define (identity-matrix size)
        (if (= 0 size) nil
            (cons (cons 1 ((repeated (partial cons 0)
                                     (dec size)) nil))
                  (map (lambda (line)
                         (cons 0 line))
                       (identity-matrix (dec size))))))]

@sicpnl[
 (define (matrix-*-vector m v)
   (map (partial dot-product v) m))
 (define (transpose mat)
   (accumulate-n cons nil mat))
 (define (matrix-*-matrix m n)
   (map (partial matrix-*-vector (transpose n)) m))]

@sicp[
 (define test-matrix
   (list (list 1 2 3 4)
         (list 4 5 6 6)
         (list 6 7 8 9)))
 (print-matrix test-matrix)
 (print-matrix (identity-matrix 4))

 (print-list (matrix-*-vector (list (list 3 0 1)
                                    (list 0 8 0)
                                    (list 1 2 3))
                              (list 3 5 7)))

 (print-matrix (transpose test-matrix))

 (print-matrix (matrix-*-matrix test-matrix
                                (identity-matrix 4)))

 (print-matrix (matrix-*-matrix test-matrix
                                (transpose test-matrix)))
 (print-matrix (matrix-*-matrix (transpose test-matrix)
                                test-matrix))]

@section{Exercise 2.38}

@sicpnl[
 (define fold-right accumulate)
 (define fold-left reduce)]

@sicp[#:label "Just evaluate them..."
      (fold-right / 1 (list 1 2 3))
      (fold-left / 1 (list 1 2 3))
      (print-list (fold-right list nil (list 1 2 3)))
      (print-list (fold-left list nil (list 1 2 3)))]

@tt{fold-left} and @tt{fold-right} will always produce the same
value for any sequence if @tt{op} is commutative.

@section{Exercise 2.39}

@sicpnl[
 (define (reverse-a sequence)
   (fold-right (lambda (x y) (append y (list x))) nil sequence))
 (define (reverse-b sequence)
   (fold-left (lambda (x y) (cons y x)) nil sequence))]

@sicp[
 (print-list (reverse-a (list 1 2 3 4 5 6)))
 (print-list (reverse-b (list 1 2 3 4 5 6)))]

@section{Exercise 2.40}

@sicp[#:hidden #t
 (define (miller-rabin n times)
   (define n-1 (dec n))
   (define (do-test a)
     (= 1 (expmod a n-1 n)))
   (define (expmod base exp m)
     (define (mulmod x y)
       (define rem (remainder (* x y) m))
       (if (and (= x y)
                (= rem 1)
                (not (= x 1))
                (not (= x (dec m))))
           0
           rem))
     (hyperop base exp mulmod 1))
   (define (loop i)
     (cond ((= i 0) #t)
           ((do-test (inc (random (dec n)))) (loop (dec i)))
           (else #f)))
   (loop times))
 (define (prime? n)
   (miller-rabin n 20))]

@sicp[#:label "Copied:"
      (define (enumerate-interval low high)
        (if (> low high)
            nil
            (cons low (enumerate-interval (inc low) high))))
      (define (prime-sum? pair)
        (prime? (+ (car pair) (cadr pair))))
      (define (make-pair-sum pair)
        (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))]

@sicpnl[
 (define (unique-pairs n)
   (if (<= n 1)
       nil
       (append (unique-pairs (dec n))
               (map (partial list n)
                    (enumerate-interval 1 (dec n))))))]

@sicp[
 (print-matrix (unique-pairs 6))]

@sicpnl[
 (define (prime-sum-pairs n)
   (map make-pair-sum
        (filter prime-sum?
                (unique-pairs 6))))]

@sicp[
 (print-matrix (prime-sum-pairs 6))]

@section{Exercise 2.41}

@sicpnl[
 (define (specific-sum-triples n s)
   (filter (sums-to? s)
           (unique-triples n)))
 (define (unique-triples n)
   (if (<= n 2)
       nil
       (append (unique-triples (dec n))
               (map (partial cons n)
                    (unique-pairs (dec n))))))
 (define (sums-to? n)
   (lambda (seq)
     (= (apply + seq) n)))]

@sicp[
 (print-matrix (unique-triples 6))
 (print-matrix (specific-sum-triples 6 11))]

@section{Exercise 2.42}

@sicp[#:label "Copied:"
      (define (flatmap proc seq)
        (accumulate append nil (map proc seq)))
      (define (queens board-size)
        (define (queen-cols k)
          (if (= k 0)
              (list empty-board)
              (filter
               (lambda (positions) (safe? k positions))
               (flatmap
                (lambda (rest-of-queens)
                  (map (lambda (new-row)
                         (adjoin-position new-row k rest-of-queens))
                       (enumerate-interval 1 board-size)))
                (queen-cols (- k 1))))))
        (queen-cols board-size))]

@sicpnl[
 (define empty-board nil)

 (define (adjoin-position row col rest)
   (cons (list row col) rest))

 (define row-of car)
 (define col-of cadr)

 (define (safe? k queens)
   (null? (filter (let ([this-queen (car queens)])
                    (lambda (other-queen)
                      (or (= (row-of this-queen)          ;; row
                             (row-of other-queen))

                          (= (col-of this-queen)          ;; col
                             (col-of other-queen))

                          (= (- (row-of this-queen)
                                (col-of this-queen))
                             (- (row-of other-queen)
                                (col-of other-queen)))    ;; diagonal \

                          (= (+ (row-of this-queen)
                                (col-of this-queen))
                             (+ (row-of other-queen)
                                (col-of other-queen)))))) ;; diagonal /

                  (cdr queens))))]

@sicp[#:label "Board drawing!"
      (define range enumerate-interval)
      (define (print-queens queens)
        (let* ([size (length queens)]
               [line (string-append "  "
                                    ((repeated (partial string-append "+---") size)
                                     "+\n"))])
          (define (pos-char row col)
            (if (null? (filter (lambda (queen)
                                 (and (= (car queen) row)
                                      (= (cadr queen) col)))
                               queens))
                #\space
                #\Q))
          (display "  ")
          (for-each (lambda (col)
                      (display "  ")
                      (display col)
                      (display " "))
                    (range 1 size))
          (newline)
          (for-each (lambda (row)
                      (display line)
                      (display row)
                      (display " ")
                      (for-each (lambda (col)
                                  (display "| ")
                                  (display (pos-char row col))
                                  (display " "))
                                (range 1 size))
                      (display "|\n"))
                    (range 1 size))
          (display line)
          (display " \n")))]

@sicp[
 (length (queens 8))
 (define six-queens (queens 6))
 (length six-queens)
 (for-each print-queens six-queens)]

@section{Exercise 2.43}

@racketblock[
 (flatmap
  (lambda (new-row)
    (map (lambda (rest-of-queens)
           (adjoin-position new-row k rest-of-queens))
         (queen-cols (- k 1))))
  (enumerate-interval 1 board-size))]

In doing this, @racket[queen-cols (- k 1)] is recalculated unnecessarily every time

@racketblock[
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position new-row k rest-of-queens))
        (queen-cols (- k 1))))]

is called.

That is, @racket[(queen-cols (k - 1))] gets called @tt{board-size} times from
@racket[(queen-cols k)], which means the time complexity of @tt{queen-cols},
and thus @tt{queens}, grows exponentially.

Therefore, Louis' program will take approximately @${n^n} times as long to compute
@racket[(queens n)] compared to the other program.

@section{Exercise 2.44}

@(define img-eval
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50]
                  [sandbox-propagate-exceptions #f])
     (make-base-eval)))

@examples[#:eval img-eval #:hidden
          ;; Have painters be Racket images, for now, and rewrite the implementation later when it's necessary.
          (require 2htdp/image
                   lang/posn
                   racket/math)
          (define wave-image (bitmap/file "chapters/two/painter-images/wave.gif"))
          (define rogers-image (bitmap/file "chapters/two/painter-images/rogers.gif"))
          (define wave wave-image)
          (define rogers rogers-image)
          (define beside0 beside)
          (define (beside left right)
            (beside0 (scale/xy 0.5 1 left)
                     (scale/xy 0.5 1 right)))
          (define (below bottom top)
            (above (scale/xy 1 0.5 top)
                   (scale/xy 1 0.5 bottom)))]

@examples[#:eval img-eval #:label "Copied:"
          (define (right-split painter n)
            (if (= n 0)
                painter
                (let ((smaller (right-split painter (- n 1))))
                  (beside painter (below smaller smaller)))))
          (define (corner-split painter n)
            (if (= n 0)
                painter
                (let ((up (up-split painter (- n 1)))
                      (right (right-split painter (- n 1))))
                  (let ((top-left (beside up up))
                        (bottom-right (below right right))
                        (corner (corner-split painter (- n 1))))
                    (beside (below painter top-left)
                            (below bottom-right corner))))))]

@examples[#:eval img-eval #:label #f
          (define (up-split painter n)
            (if (= n 0)
                painter
                (let ((smaller (up-split painter (- n 1))))
                  (below painter (beside smaller smaller)))))]

@examples[#:eval img-eval
          (up-split wave 4)
          (up-split rogers 4)
          (corner-split wave 4)
          (corner-split rogers 4)]

@section{Exercise 2.45}

@examples[#:eval img-eval #:label #f
          (define (split final-combiner
                         smaller-combiner)
            (define (loop painter n)
              (if (= n 0)
                  painter
                  (let ([smaller (loop painter (- n 1))])
                    (final-combiner painter
                                    (smaller-combiner smaller smaller)))))
            loop)

          (define right-split (split beside below))
          (define up-split (split below beside))]

@examples[#:eval img-eval
          (up-split wave 4)
          (up-split rogers 4)
          (corner-split wave 4)
          (corner-split rogers 4)]

@section{Exercise 2.46}

@examples[#:eval img-eval #:label #f
          (define (make-vect x y)
            (list x y))
          (define xcor-vect car)
          (define ycor-vect cadr)]

@examples[#:eval img-eval #:label #f
          (define (add-vect a b)
            (make-vect (+ (xcor-vect a)
                          (xcor-vect b))
                       (+ (ycor-vect a)
                          (ycor-vect b))))

          (define (sub-vect a b)
            (make-vect (- (xcor-vect a)
                          (xcor-vect b))
                       (- (ycor-vect a)
                          (ycor-vect b))))

          (define (scale-vect s vect)
            (make-vect (* (xcor-vect vect)
                          s)
                       (* (ycor-vect vect)
                          s)))

          (define (print-vect vect)
            (display "[")
            (display (xcor-vect vect))
            (display " ")
            (display (ycor-vect vect))
            (display "]"))]

@examples[#:eval img-eval
          (print-vect (make-vect 0 0))
          (print-vect (add-vect (make-vect 1 2)
                                (make-vect 10 20)))
          (print-vect (sub-vect (make-vect 10 20)
                                (make-vect 1 2)))
          (print-vect (scale-vect 10 (make-vect 1 2)))]

@section{Exercise 2.47}

@examples[#:eval img-eval #:label "Display a frame using the abstractions..."
          (define (print-frame frame)
            (display "Origin: ")
            (print-vect (origin-frame frame))
            (newline)
            (display "Edge 1: ")
            (print-vect (edge1-frame frame))
            (newline)
            (display "Edge 2: ")
            (print-vect (edge2-frame frame))
            (newline))]

@examples[#:eval img-eval #:label "First implementation:"
          (define (make-frame origin edge1 edge2)
            (list origin edge1 edge2))
          (define origin-frame car)
          (define edge1-frame cadr)
          (define edge2-frame caddr)]

@examples[#:eval img-eval
          (print-frame (make-frame (make-vect 0 0)
                                   (make-vect 1 0)
                                   (make-vect 0 1)))]

@examples[#:eval img-eval #:label "Second implementation:"
          (define (make-frame origin edge1 edge2)
            (cons origin (cons edge1 edge2)))
          (define origin-frame car)
          (define edge1-frame cadr)
          (define edge2-frame cddr)]

@examples[#:eval img-eval
          (print-frame (make-frame (make-vect 0 0)
                                   (make-vect 1 0)
                                   (make-vect 0 1)))]

@section{Exercise 2.48}

@examples[#:eval img-eval #:label #f
          (define make-segment cons)
          (define start-segment car)
          (define end-segment cdr)]

@section{Exercise 2.49}

@examples[#:eval img-eval #:hidden
          (define canvas empty-image)
          (define (drawing drawer)
            (begin (set! canvas empty-image)
                   (drawer)
                   (flip-vertical canvas))) ;; make (0, 0) be bottom-left
          (define (draw-line from to)
            (set! canvas
                  (add-line canvas
                            (xcor-vect from)
                            (ycor-vect from)
                            (xcor-vect to)
                            (ycor-vect to)
                            "black")))
          (define (partial proc . args0)
            (lambda args
              (apply proc (append args0 args))))]

@examples[#:eval img-eval #:label "Copied:"
          (define (frame-coord-map frame)
            (lambda (v)
              (add-vect
               (origin-frame frame)
               (add-vect (scale-vect (xcor-vect v)
                                     (edge1-frame frame))
                         (scale-vect (ycor-vect v)
                                     (edge2-frame frame))))))
          (define (segments->painter segment-list)
            (lambda (frame)
              (for-each
               (lambda (segment)
                 (draw-line
                  ((frame-coord-map frame) (start-segment segment))
                  ((frame-coord-map frame) (end-segment segment))))
               segment-list)))]

Where @tt{drawing} executes a 0 argument procedure,
and any painters applied from that procedure paint onto
a blank canvas, which is then returned.

@examples[#:eval img-eval #:label #f
          (define test-frame
            (make-frame (make-vect 0 0)
                        (make-vect 200 0)
                        (make-vect 0 200)))

          (define (lines x0 y0
                         x1 y1
                         . rest)
            (cons (make-segment (make-vect x0 y0)
                                (make-vect x1 y1))
                  (if (null? rest)
                      null
                      (apply lines
                             x1 y1
                             rest))))

          (drawing (partial
                    (segments->painter (lines 0 0
                                              0 1
                                              1 1
                                              1 0
                                              0 0))
                    test-frame))
          (drawing (partial
                    (segments->painter (append (lines 0 0
                                                      1 1)
                                               (lines 0 1
                                                      1 0)))
                    test-frame))
          (drawing (partial
                    (segments->painter (lines 0.0 0.5
                                              0.5 1.0
                                              1.0 0.5
                                              0.5 0.0
                                              0.0 0.5))
                    test-frame))

          (define wave
            (segments->painter (append (lines 0.0  0.85
                                              0.15 0.60
                                              0.3  0.65
                                              0.4  0.65
                                              0.35 0.85
                                              0.4  1.0)
                                       (lines 0.6  1.0
                                              0.65 0.85
                                              0.6  0.65
                                              0.75 0.65
                                              1.0  0.35)
                                       (lines 1.0  0.15
                                              0.6  0.45
                                              0.75 0.0)
                                       (lines 0.6  0.0
                                              0.5  0.3
                                              0.4  0.0)
                                       (lines 0.25 0.0
                                              0.35 0.5
                                              0.3  0.6
                                              0.15 0.4
                                              0.0  0.65))))
          (drawing (partial wave test-frame))]

@section{Exercise 2.50}

@examples[#:eval img-eval #:label "Copied:"
          (define (transform-painter painter origin corner1 corner2)
            (lambda (frame)
              (let ((m (frame-coord-map frame)))
                (let ((new-origin (m origin)))
                  (painter
                   (make-frame new-origin
                               (sub-vect (m corner1) new-origin)
                               (sub-vect (m corner2) new-origin)))))))
          (define (flip-vert painter)
            (transform-painter painter
                               (make-vect 0.0 1.0)   ; new origin
                               (make-vect 1.0 1.0)   ; new end of edge1
                               (make-vect 0.0 0.0))) ; new end of edge2

          (define (rotate90 painter)
            (transform-painter painter
                               (make-vect 1.0 0.0)
                               (make-vect 1.0 1.0)
                               (make-vect 0.0 0.0)))

          (define (beside painter1 painter2)
            (let ((split-point (make-vect 0.5 0.0)))
              (let ((paint-left
                     (transform-painter painter1
                                        (make-vect 0.0 0.0)
                                        split-point
                                        (make-vect 0.0 1.0)))
                    (paint-right
                     (transform-painter painter2
                                        split-point
                                        (make-vect 1.0 0.0)
                                        (make-vect 0.5 1.0))))
                (lambda (frame)
                  (paint-left frame)
                  (paint-right frame)))))]

@examples[#:eval img-eval #:label #f
          (define (flip-horiz painter)
            (transform-painter painter
                               (make-vect 1.0 0.0)
                               (make-vect 0.0 0.0)
                               (make-vect 1.0 1.0)))
          (define (rotate180 painter)
            (transform-painter painter
                               (make-vect 1.0 1.0)
                               (make-vect 0.0 1.0)
                               (make-vect 1.0 0.0)))
          (define (rotate270 painter)
            (transform-painter painter
                               (make-vect 0.0 1.0)
                               (make-vect 0.0 0.0)
                               (make-vect 1.0 1.0)))]

@examples[#:eval img-eval
          (drawing (partial (flip-horiz wave) test-frame))
          (drawing (partial (rotate180 wave) test-frame))
          (drawing (partial (rotate270 wave) test-frame))]

@section{Exercise 2.51}

@examples[#:eval img-eval #:label "Analogous:"
          (define (below painter1 painter2)
            (let ((split-point (make-vect 0.0 0.5)))
              (let ((paint-top
                     (transform-painter painter1
                                        (make-vect 0.0 0.0)
                                        (make-vect 1.0 0.0)
                                        split-point))
                    (paint-bottom
                     (transform-painter painter2
                                        split-point
                                        (make-vect 1.0 0.5)
                                        (make-vect 0.0 1.0))))
                (lambda (frame)
                  (paint-top frame)
                  (paint-bottom frame)))))]

@examples[#:eval img-eval
          (drawing (partial (below wave
                                   (flip-horiz wave))
                            test-frame))]

In terms of @tt{rotation}s and @tt{below}:

@examples[#:eval img-eval #:label #f
          (define (below painter1 painter2)
            (rotate90 (beside (rotate270 painter1)
                              (rotate270 painter2))))]

@examples[#:eval img-eval
          (drawing (partial (below wave
                                   (flip-horiz wave))
                            test-frame))]

@section{Exercise 2.52}

@examples[#:eval img-eval #:hidden
          (define (identity thing) thing)

          ;; rebind these
          (define right-split (split beside below))
          (define up-split (split below beside))]

@examples[#:eval img-eval #:label "Copied:"
          (define (square-of-four tl tr bl br)
            (lambda (painter)
              (let ((top (beside (tl painter) (tr painter)))
                    (bottom (beside (bl painter) (br painter))))
                (below bottom top))))

          (define (square-limit painter n)
            (let ((combine4 (square-of-four flip-horiz identity
                                            rotate180 flip-vert)))
              (combine4 (corner-split painter n))))]

@examples[#:eval img-eval
          (define larger-frame (make-frame (make-vect 0 0)
                                           (make-vect 300 0)
                                           (make-vect 0 300)))
          (define (wave-limit)
            (drawing (partial (square-limit wave 4) larger-frame)))
          (wave-limit)]

@subsection{Exercise 2.52.a}

@examples[#:eval img-eval #:label #f
          (define wave0 wave)
          (define wave (lambda (frame)
                         (smile frame)
                         (wave0 frame)))
          (define smile
            (segments->painter (append (lines 0.4  0.8
                                              0.45 0.75
                                              0.55 0.75
                                              0.6  0.8)

                                       (lines 0.39 0.9
                                              0.4  0.91
                                              0.41 0.9
                                              0.4  0.89
                                              0.39 0.9)
                                       (lines 0.59 0.9
                                              0.6  0.91
                                              0.61 0.9
                                              0.6  0.89
                                              0.59 0.9))))]

@examples[#:eval img-eval
          (wave-limit)]

@examples[#:eval img-eval #:label #f
          (define wave wave0)]

@subsection{Exercise 2.52.b}

@examples[#:eval img-eval #:label #f
          (define corner-split0 corner-split)

          (define (corner-split painter n)
            (if (= n 0)
                painter
                (let ((up (up-split painter (- n 1)))
                      (right (right-split painter (- n 1))))
                  (let ((top-left up)
                        (bottom-right right)
                        (corner (corner-split painter (- n 1))))
                    (beside (below painter top-left)
                            (below bottom-right corner))))))]

@examples[#:eval img-eval
          (wave-limit)]

@examples[#:eval img-eval #:label #f
          (define corner-split corner-split0)]

@subsection{Exercise 2.52.c}

@examples[#:eval img-eval #:label #f
          (define square-limit0 square-limit)

          (define (square-limit painter n)
            (let ((combine4 (square-of-four flip-vert rotate180
                                            identity flip-horiz)))
              (combine4 (corner-split painter n))))]

@examples[#:eval img-eval
          (wave-limit)]

@examples[#:eval img-eval #:label #f
          (define square-limit square-limit0)]
