#lang scribble/manual
@(require racket/sandbox
          scribble/example
          scribble-math/dollar
          "../sicp-eval.rkt")

@title[#:style (with-html5 manual-doc-style)]{Assignment and Local State}
@(use-katex)

@section{Exercise 3.1}

@sicpnl[(define (make-accumulator total)
          (lambda (value)
            (set! total (+ total value))
            total))]

@sicp[(define A (make-accumulator 5))
      (A 10)
      (A 10)]
