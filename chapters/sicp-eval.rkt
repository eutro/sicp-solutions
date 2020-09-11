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
  (sicp synx)
  (display "Macroexpanding 'sicp at ")
  (display (syntax-line synx))
  (display ":")
  (display (syntax-column synx))
  (display ":")
  (display (syntax-source synx))
  (newline)
  (datum->syntax #'sicp `(examples #:eval sicp-evaluator ,@(cdr (syntax->list synx)))))

(define-syntax-rule
  (sicpnl exp ...)
  (sicp #:label #f exp ...))
