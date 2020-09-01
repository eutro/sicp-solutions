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
