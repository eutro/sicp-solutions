#lang racket

(require racket/port
         scribble-math/asymptote)

(define (asymptote-script)
  (define (coord x y)
    (begin (display "(")
           (display x)
           (display ",")
           (display y)
           (display ")")))
  
  (define (box-at x y)
    (begin (display "draw(box(")
           (coord x y)
           (display ",")
           (coord (+ x 30)
                  (- y 30))
           (display "));\n")))

  (define (arrow x0 y0
                 x1 y1)
    (begin (display "draw(")
           (coord x0 y0)
           (display "--")
           (coord x1 y1)
           (display ",Arrow);\n")))

  (define (label x y text offset)
    (begin (display "label(\"")
           (display text)
           (display "\",")
           (coord x y)
           (display offset)
           (display ");\n")))

  (define (cross x y)
    (begin (display "draw(")
           (coord x (- y 30))
           (display "--")
           (coord (+ x 30) y)
           (display ");\n")))

  (define (dot-in x y)
    (begin (display "fill(")
           (coord (+ x 10) (- y 15))
           (display "..")
           (coord (+ x 15) (- y 10))
           (display "..")
           (coord (+ x 20) (- y 15))
           (display "..")
           (coord (+ x 15) (- y 20))
           (display "..cycle);\n")))

  (define (draw-at x y l)
    (begin (label (- x 20)
                  (+ y 20)
                  l ",NW")

           (arrow (- x 20) (+ y 20)
                  (- x 2)  (+ y 2))

           (box-at x y)
           (dot-in x y)
           (arrow (+ x 15)
                  (- y 15)
                  (+ x 15)
                  (- y 80))
           (draw-node x (- y 80)
                      (car l))

           (box-at (+ x 30)
                   y)

           (if (null? (cdr l))
               (cross (+ x 30) y)
               (begin (dot-in (+ x 30) y)
                      (arrow (+ x 45)
                             (- y 15)
                             (+ x 120)
                             (- y 15))
                      (draw-node (+ x 120) y
                                 (cdr l))))))

  (define (draw-val x y val)
    (begin (box-at x y)
           (label (+ x 15)
                  (- y 15)
                  val "")))

  (define (draw-node x y node)
    ((if (pair? node)
         draw-at
         draw-val) x y node))

  (with-output-to-string (lambda ()
                           (draw-at 0 0
                                    (list 1 (list 2 (list 3 4)))))))

(define tree (list 1 (list 2 (list 3 4))))

(asymptote (asymptote-script))
