#lang racket/base
(require racket/sandbox
         scribble/example
         (for-syntax racket/base))
(provide sicp
         sicpnl
         sicp-evaluator
         start-sicp-repl)

(define sicp-repl? (getenv "SICP_REPL"))

(define sicp-evaluator
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 100]
                 [sandbox-propagate-exceptions #f])
    (make-evaluator 'sicp)))

(define (start-sicp-repl)
  (when sicp-repl?
    (displayln "SICP REPL")
    (parameterize ([current-eval sicp-evaluator]
                   [current-print
                    (let ([prev (current-print)])
                      (lambda args
                        (let ([out (get-output sicp-evaluator)]
                              [eout (get-error-output sicp-evaluator)])
                          (display out)
                          (display eout (current-error-port))
                          (when (not (equal? out ""))
                              (newline))
                          (apply prev args))))])
      (read-eval-print-loop))))

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
