#lang racket/base
(require racket/sandbox
         scribble/example)
(provide sicp
         sicpnl
         sicp-evaluator)

(define sicp-evaluator
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50]
                 [sandbox-propagate-exceptions #f])
    (make-evaluator 'sicp)))

(define-syntax-rule
  (sicp exp ...)
  (examples #:eval sicp-evaluator exp ...))

(define-syntax-rule
  (sicpnl exp ...)
  (sicp #:label #f exp ...))
