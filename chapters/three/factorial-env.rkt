#lang racket

(require pict threading "env.rkt")

(define ((constant-x x) _ y)
  (values x y))

(export-pict
 (let ([ge (env "Global Env" 35 20 '())]
       [e1 (env "E1" 25 20 '((n . 6)))]
       [e2 (env "E2" 25 20 '((n . 5)))]
       [e3 (env "E3" 25 20 '((n . 4)))]
       [e4 (env "E4" 25 20 '((n . 3)))]
       [e5 (env "E5" 25 20 '((n . 2)))]
       [e6 (env "E6" 25 20 '((n . 1)))])
   (~> (blank 512 142)
       (pin-over 40 10 ge)
       (pin-over 10 50 (text (~a '(factorial 6)) 'modern))
       (pin-over 90 80 e1)
       (pin-over 140 60 e2)
       (pin-over 190 80 e3)
       (pin-over 240 60 e4)
       (pin-over 290 80 e5)
       (pin-over 340 60 e6)
       (pin-arrow-line 10 _
                       e1 (compose (constant-x 120) ct-find)
                       ge (compose (constant-x 120) lb-find))
       (pin-arrow-line 10 _
                       e2 cb-find
                       e1 rc-find)
       (pin-arrow-line 10 _
                       e3 ct-find
                       e2 rc-find)
       (pin-arrow-line 10 _
                       e4 cb-find
                       e3 rc-find)
       (pin-arrow-line 10 _
                       e5 ct-find
                       e4 rc-find)
       (pin-arrow-line 10 _
                       e6 cb-find
                       e5 rc-find)
       (pin-over 100 106 (text "(if (= n 1)" 'modern))
       (pin-over 100 118 (text "    1" 'modern))
       (pin-over 100 130 (text "    (* n (factorial (- n 1))))" 'modern))))
 "diagrams/factorial-recur.png")

(export-pict
 (let ([ge (env "Global Env" 500 20 '())]
       [e1 (env "E1" 30 20 '((n . 6)))]
       [e2 (env "E2" 85 40 '((product . 1)
                             (counter . 1)
                             (max-count . 6)))]
       [e3 (env "E3" 85 40 '((product . 1)
                             (counter . 2)
                             (max-count . 6)))]
       [e4 (env "E4" 85 40 '((product . 2)
                             (counter . 3)
                             (max-count . 6)))]
       [e5 (env "E5" 85 40 '((product . 6)
                             (counter . 4)
                             (max-count . 6)))]
       [e6 (env "E6" 85 40 '((product . 24)
                             (counter . 5)
                             (max-count . 6)))]
       [e7 (env "E7" 85 40 '((product . 120)
                             (counter . 6)
                             (max-count . 6)))]
       [e8 (env "E8" 85 40 '((product . 720)
                             (counter . 7)
                             (max-count . 6)))])
   (~> (blank 768 228)
       (pin-over 40 10 ge)
       (pin-over 10 50 (text (~a '(factorial 6)) 'modern))
       (pin-over 90 80 e1)
       (pin-over 130 120 e2)
       (pin-over 190 70 e3)
       (pin-over 250 120 e4)
       (pin-over 310 70 e5)
       (pin-over 370 120 e6)
       (pin-over 430 70 e7)
       (pin-over 500 120 e8)
       (pin-arrow-line 10 _
                       e1 (compose (constant-x 120) ct-find)
                       ge (compose (constant-x 120) lb-find))
       (pin-arrow-line 10 _
                       e2 (compose (constant-x 180) ct-find)
                       ge (compose (constant-x 180) lb-find))
       (pin-arrow-line 10 _
                       e3 (compose (constant-x 240) ct-find)
                       ge (compose (constant-x 240) lb-find))
       (pin-arrow-line 10 _
                       e4 (compose (constant-x 300) ct-find)
                       ge (compose (constant-x 300) lb-find))
       (pin-arrow-line 10 _
                       e5 (compose (constant-x 360) ct-find)
                       ge (compose (constant-x 360) lb-find))
       (pin-arrow-line 10 _
                       e6 (compose (constant-x 420) ct-find)
                       ge (compose (constant-x 420) lb-find))
       (pin-arrow-line 10 _
                       e7 (compose (constant-x 490) ct-find)
                       ge (compose (constant-x 490) lb-find))
       (pin-arrow-line 10 _
                       e8 (compose (constant-x 560) ct-find)
                       ge (compose (constant-x 560) lb-find))
       (pin-over 20 106 (text (~a '(fact-iter 1 1 n)) 'modern))
       (pin-over 20 166 (text "(if (> counter max-count)" 'modern))
       (pin-over 20 178 (text "    product" 'modern))
       (pin-over 20 190 (text "    (fact-iter (* counter product)" 'modern))
       (pin-over 20 202 (text "               (+ counter 1)" 'modern))
       (pin-over 20 214 (text "               max-count))" 'modern))))
 "diagrams/factorial-iter.png")
