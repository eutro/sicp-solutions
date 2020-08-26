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
                  (display this)
                  (display "\");\n")))

         (define (visit node parent)
           (if (pair? node)
               (traverse node parent)
               (make-node parent node)))
         
         (define (traverse l parent)
           (if (null? l)
               null
               (begin (make-node parent l)
                      (visit (car l) l)
                      (traverse (cdr l) l))))

         (with-output-to-string (lambda ()
                                  (let ([tree (list 1 (list 2 (list 3 4)))])
                                    (traverse tree null)
                                    (display "draw(")
                                    (display (format-node tree))
                                    (display ", (0,0));"))))))

(asymptote "import drawtree;"
           "size(4cm, 0);"
           (asymptote-script))
