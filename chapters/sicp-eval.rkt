#lang racket/base
(require racket/sandbox
         scribble/example
         (for-syntax racket/base))
(provide sicp
         sicpnl
         sicp-evaluator)

(define sicp-evaluator
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50]
                 [sandbox-propagate-exceptions #f])
    (make-evaluator 'sicp)))

(define-syntax
  (sicp arg)
  (display "Evaluating Scheme at ")
  (display (syntax-line arg))
  (display ":")
  (display (syntax-column arg))
  (display ":")
  (display (syntax-source arg))
  (newline)
  #'(examples #:eval sicp-evaluator ,arg))

(define-syntax-rule
  (sicpnl exp ...)
  (sicp #:label #f exp ...))
