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
