#lang racket

(require racket/port
         scribble-math/asymptote
         racket/format)

(define (asymptote-script)
  (begin (define (format-node node)
           (string-replace
            (string-replace
             (string-replace
              (~a "node" node)
              " " "t")
             ")" "_")
            "(" "_"))

         (define (make-node parent this)
           (begin (display "TreeNode ")
                  (display (format-node this))
                  (display " = makeNode(")
                  (if (null? parent)
                      (display "\"")
                      (begin (display (format-node parent))
                             (display ", \"")))
                  (display (car this))
                  (display "\");\n")))
         
         (define (traverse l parent)
           (if (null? l)
               (make-node parent '("" () ()))
               (begin (make-node parent l)
                      (traverse (cadr l) l)
                      (traverse (caddr l) l))))

         (with-output-to-string (lambda ()
                                  (let ([tree '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))])
                                    (traverse tree null)
                                    (display "draw(")
                                    (display (format-node tree))
                                    (display ", (0,0));"))))))

(asymptote "import drawtree;"
           "size(4cm, 0);"
           (asymptote-script))
