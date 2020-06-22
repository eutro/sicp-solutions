#lang racket

(require racket/port
         scribble-math/asymptote)

(define (asymptote-script)
  (begin (define (make-node parent name text)
           (begin (display "TreeNode ")
                  (display name)
                  (display " = makeNode(")
                  (display parent)
                  (display ", \"")
                  (display text)
                  (display "\");")))

         (define (count-change amount)
           (begin (display (string-append "TreeNode root = makeNode(\"count-change "
                                          (number->string amount)
                                          "\");"))
                  (cc amount 5 "root")))

         (define (cc amount kinds-of-coins parent)
           (begin (define name (string-append parent
                                              "__"
                                              (number->string (if (< amount 0)
                                                                  (* -100 amount)
                                                                  amount))
                                              "_"
                                              (number->string kinds-of-coins)))
                  (make-node parent
                             name
                             (string-append "cc "
                                            (number->string amount)
                                            " "
                                            (number->string kinds-of-coins)))
                  (cond ((= amount 0) (begin (make-node name
                                                        "_"
                                                        1)
                                             1))
                        ((or (< amount 0) (= kinds-of-coins 0)) (begin (make-node name
                                                                                  "_"
                                                                                  0)
                                                                       0))
                        (else (+ (cc amount
                                     (- kinds-of-coins 1)
                                     name)
                                 (cc (- amount
                                        (first-denomination kinds-of-coins))
                                     kinds-of-coins
                                     name))))))

         (define (first-denomination kinds-of-coins)
           (cond ((= kinds-of-coins 1) 1)
                 ((= kinds-of-coins 2) 5)
                 ((= kinds-of-coins 3) 10)
                 ((= kinds-of-coins 4) 25)
                 ((= kinds-of-coins 5) 50)))

         (with-output-to-string (lambda ()
                                  (count-change 11)))))

(asymptote "import drawtree;"
           "size(4cm, 0);"
           (asymptote-script)
           "draw(root, (0,0));")