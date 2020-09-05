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

@section{Exercise 2.62}

@sicpnl[(define (union-set a b)
          (cond [(null? a) b]
                [(null? b) a]
                [(= (car a)
                    (car b))
                 (cons (car a)
                       (union-set (cdr a)
                                  (cdr b)))]
                [(< (car a)
                    (car b))
                 (cons (car a)
                       (union-set (cdr a)
                                  b))]
                [else
                 (cons (car b)
                       (union-set a
                                  (cdr b)))]))]

@sicp[(print-set (union-set '(1 2 3 5 7 9)
                            '(2 3 4 6 8 9)))]

@section{Exercise 2.63}

@sicp[#:label "Copied:"
      (define (entry tree) (car tree))
      (define (left-branch tree) (cadr tree))
      (define (right-branch tree) (caddr tree))
      (define (make-tree entry left right)
        (list entry left right))

      (define (element-of-set? x set)
        (cond ((null? set) false)
              ((= x (entry set)) true)
              ((< x (entry set))
               (element-of-set? x (left-branch set)))
              ((> x (entry set))
               (element-of-set? x (right-branch set)))))

      (define (adjoin-set x set)
        (cond ((null? set) (make-tree x '() '()))
              ((= x (entry set)) set)
              ((< x (entry set))
               (make-tree (entry set)
                          (adjoin-set x (left-branch set))
                          (right-branch set)))
              ((> x (entry set))
               (make-tree (entry set)
                          (left-branch set)
                          (adjoin-set x (right-branch set))))))

      (define (tree->list-1 tree)
        (if (null? tree)
            '()
            (append (tree->list-1 (left-branch tree))
                    (cons (entry tree)
                          (tree->list-1 (right-branch tree))))))
      (define (tree->list-2 tree)
        (define (copy-to-list tree result-list)
          (if (null? tree)
              result-list
              (copy-to-list (left-branch tree)
                            (cons (entry tree)
                                  (copy-to-list (right-branch tree)
                                                result-list)))))
        (copy-to-list tree '()))]

@subsection{Exercise 2.63.a}

They will always produce the ordered list of the elements in the set.

@tt{tree->list-1} appends, to list generated by the left branch,
the list generated by the right branch with the value of the
node prepended.

@$${(\text{list generated by left branch}) + [\text{value of node} + (\text{list generated by right branch})]}

@tt{tree->list-2} does the same, but iteratively, and without using
@tt{append}.

The value at the node will always be more than any of the values
in the left branch, and will always be less than any of the values
in the right branch. Thus, collecting in this way will always
produce a sorted list.

@sicpnl[(define unbalanced-tree
          (fold-right adjoin-set
                      nil
                      '(7 6 5 4 3 2 1)))

        (print-list unbalanced-tree)]

@sicp[#:label "They produce the following lists on the unbalanced tree:"
      (print-list (tree->list-1 unbalanced-tree))
      (print-list (tree->list-2 unbalanced-tree))]

@subsection{Exercise 2.63.b}

@tt{tree->list-1} is @${O(n^2)} at worst, and @${O(n)} at best,
with the worst case being that all right subtrees are empty, and the
best case being that all the left subtrees are empty.

This is because @tt{append} is @${O(n)}, where @${n} is the length of
the first list, and is called for each node, with the list generated
by the left branch as the first list.

@tt{tree->list-2} is @${\Theta(n)}, since it visits each node once,
and uses @tt{cons} (assumed to be @${O(1)}) to prepend each entry
to the rest of the list.

@section{Exercise 2.64}

@sicp[#:label "Copied:"
      (define (list->tree elements)
        (car (partial-tree elements (length elements))))]

@sicp[#:label "Modified for readability:"
      (define (partial-tree elts n)
        (if (= n 0)
            (cons '() elts)
            (let* ([left-size (quotient (- n 1) 2)]
                   [left-result (partial-tree elts left-size)]
                   [left-tree (car left-result)]
                   [non-left-elts (cdr left-result)]
                   [right-size (- n (+ left-size 1))]
                   [this-entry (car non-left-elts)]
                   [right-result (partial-tree (cdr non-left-elts)
                                               right-size)]
                   [right-tree (car right-result)]
                   [remaining-elts (cdr right-result)])
              (cons (make-tree this-entry
                               left-tree
                               right-tree)
                    remaining-elts))))]

@subsection{Exercise 2.64.a}

@tt{partial-tree} takes a sorted list @${v} of node values and a number @${n}.
It returns a pair of:
@itemlist[@item{A tree using the first @${n} elements of the list.}
          @item{The rest of the elements of the list.}]

To start, the size of the left branch is computed as half of @${n - 1}, rounded down. Then,
@tt{partial-tree} is invoked on @${v} to compute the left branch, and to gather the
rest of the elements of the list. The value of the current node comes from the first element
of the list returned. @tt{partial-tree} is invoked on the @tt{cdr} of the list returned
to get the right branch and the remaining values of the list. The tree is created from
the computed value, the left branch and the right branch, then the pair is returned
with that tree and the remaining values.

As the base case, for @${n = 0}, an empty branch and the whole list is returned.

@sicpnl[(print-list (list->tree '(1 3 5 7 9 11)))]

Becomes:

@image["chapters/two/asymptote-images/bf7049b9e27295e0ce1c54abb2d03a3c"
       #:scale 1.5
       #:suffixes (list ".svg"
                        ".png"
                        ".pdf")]

@subsection{Exercise 2.64.b}

@tt{list->tree} is of time complexity @${\Theta(n)} for an input list of length @${n},
as doubling the input length adds another layer to the tree, which requires another
layer of recursive calls, doubling their number and thus doubling the time taken.

@section{Exercise 2.65}

@sicpnl[(define set-elements tree->list-2)]

@sicp[#:label "For set union, the ordered list implementation can be used."
      (define (ordered-set-union a b)
        (cond [(null? a) b]
              [(null? b) a]
              [(= (car a)
                  (car b))
               (cons (car a)
                     (ordered-set-union (cdr a)
                                        (cdr b)))]
              [(< (car a)
                  (car b))
               (cons (car a)
                     (ordered-set-union (cdr a)
                                        b))]
              [else
               (cons (car b)
                     (ordered-set-union a
                                        (cdr b)))]))

      (define (union-set set1 set2)
        (list->tree (ordered-set-union (set-elements set1)
                                       (set-elements set2))))]

@tt{set-elements} (or @tt{tree->list-2}), @tt{list->tree} and @tt{ordered-set-union} are all
@${\Theta(n)}. Thus, @tt{union-set} is also @${\Theta(n)}.

@sicp[(print-set (union-set (list->tree '(1 3 5 7 9))
                            (list->tree '(1 2 3 4 6))))]

@sicp[#:label "The same is true for set intersection."
      (define (ordered-set-intersection a b)
        (cond [(or (null? a)
                   (null? b)) '()]
              [(= (car a)
                  (car b))
               (cons (car a)
                     (ordered-set-intersection (cdr a)
                                               (cdr b)))]
              [(< (car a)
                  (car b))
               (ordered-set-intersection (cdr a)
                                         b)]
              [else
               (ordered-set-intersection a
                                         (cdr b))]))

      (define (intersection-set set1 set2)
        (list->tree (ordered-set-intersection (set-elements set1)
                                              (set-elements set2))))]

@sicp[(print-set (intersection-set (list->tree '(1 3 5 7 9))
                                   (list->tree '(1 2 3 4 6))))]

@section{Exercise 2.66}

@sicp[#:label "First, define some selectors and constructors:"
      (define node-key car)
      (define node-val cadr)
      (define node-left caddr)
      (define node-right cadddr)
      (define (make-node key val left right)
        (list key val left right))

      (define (assoc-tree tree-map key val)
        (cond [(null? tree-map)
               (make-node key
                          val
                          '()
                          '())]
              [(= (node-key tree-map)
                  key)
               (make-node key
                          val
                          (node-left tree-map)
                          (node-right tree-map))]

              [(< (node-key tree-map)
                  key)
               (make-node (node-key tree-map)
                          (node-val tree-map)
                          (node-left tree-map)
                          (assoc-tree (node-right tree-map)
                                      key val))]

              [else
               (make-node (node-key tree-map)
                          (node-val tree-map)
                          (assoc-tree (node-left tree-map)
                                      key val)
                          (node-right tree-map))]))

      (define (assoc-tree* tree-map key val . kvs)
        (let ([new-tree (assoc-tree tree-map key val)])
          (if (null? kvs)
              new-tree
              (apply assoc-tree* new-tree kvs))))

      (define (print-tree-map tree-map)
        (define (print-entries node)
          (if (null? node) '()
              (begin (print-entries (node-left node))
                     (newline)
                     (display "  ")
                     (display (node-key node))
                     (display ": ")
                     (write (node-val node))
                     (print-entries (node-right node)))))
        (display "{")
        (print-entries tree-map)
        (newline)
        (display "}"))]

@sicp[#:label "Then the lookup function:"
      (define (lookup key tree-map)
        (cond [(null? tree-map) '()]

              [(= (node-key tree-map)
                  key)
               (node-val tree-map)]

              [(< (node-key tree-map)
                  key)
               (lookup key (node-right tree-map))]

              [else
               (lookup key (node-left tree-map))]))]

@sicp[(define test-map
        (assoc-tree* '()
                     5 "five"
                     7 "seven"
                     9 "nine"
                     10 "ten"
                     8 "eight"
                     6 "six"
                     3 "three"
                     1 "one"
                     2 "two"
                     0 "zero"
                     4 "four"))

      (print-tree-map test-map)

      (lookup 2 test-map)
      (lookup 5 test-map)
      (lookup 11 test-map)]

@section{Exercise 2.67}

@sicp[#:label "Copied:"
      (define (make-leaf symbol weight)
        (list 'leaf symbol weight))
      (define (leaf? object)
        (eq? (car object) 'leaf))
      (define (symbol-leaf x) (cadr x))
      (define (weight-leaf x) (caddr x))

      (define (make-code-tree left right)
        (list left
              right
              (append (symbols left) (symbols right))
              (+ (weight left) (weight right))))

      (define (left-branch tree) (car tree))

      (define (right-branch tree) (cadr tree))
      (define (symbols tree)
        (if (leaf? tree)
            (list (symbol-leaf tree))
            (caddr tree)))
      (define (weight tree)
        (if (leaf? tree)
            (weight-leaf tree)
            (cadddr tree)))

      (define (decode bits tree)
        (define (decode-1 bits current-branch)
          (if (null? bits)
              '()
              (let ((next-branch
                     (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch)
                          (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch)))))
        (decode-1 bits tree))
      (define (choose-branch bit branch)
        (cond ((= bit 0) (left-branch branch))
              ((= bit 1) (right-branch branch))
              (else (error "bad bit -- CHOOSE-BRANCH" bit))))

      (define (adjoin-set x set)
        (cond ((null? set) (list x))
              ((< (weight x) (weight (car set))) (cons x set))
              (else (cons (car set)
                          (adjoin-set x (cdr set))))))

     (define (make-leaf-set pairs)
       (if (null? pairs)
           '()
           (let ((pair (car pairs)))
             (adjoin-set (make-leaf (car pair)
                                    (cadr pair))
                         (make-leaf-set (cdr pairs))))))

      (define sample-tree
        (make-code-tree (make-leaf 'A 4)
                        (make-code-tree
                         (make-leaf 'B 2)
                         (make-code-tree (make-leaf 'D 1)
                                         (make-leaf 'C 1)))))

      (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))]

@sicp[#:label "For printing the message:"
      (define (message->string symbol-list)
        (apply string-append
               (map symbol->string symbol-list)))]

@sicp[#:label "Decoded:"
      (message->string (decode sample-message sample-tree))]
