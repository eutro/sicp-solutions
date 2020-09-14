#lang scribble/manual
@(require racket/sandbox
          scribble/example
          scribble-math/dollar
          "../sicp-eval.rkt")

@title[#:style (with-html5 manual-doc-style)]{Systems with Generic Operations}
@(use-katex)

@section{Exercise 2.77}

The evaluation looks like this:

@racketblock[(magnitude z)
             (apply-generic 'magnitude z) (code:comment @#,code{dispatches on '(complex)})
             (magnitude (contents z))
             (magnitude (cdr z))
             (apply-generic 'magnitude (cdr z)) (code:comment @#,code{dispatches on '(rectangular)})
             (sqrt (+ (square (real-part (cddr z)))
                      (square (imag-part (cddr z))))) (code:comment @#,code{from rectangular package})
             (sqrt (+ (square (caddr z))
                      (square (cdddr z))))
             (sqrt (+ 9
                      16))
             (sqrt 25)
             5]
