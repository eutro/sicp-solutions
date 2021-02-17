#lang racket

(require pict file/convertible)

(provide export-pict env)

(define line-spacing 10)
(define line-height 8)
(define padding 2)

(define/contract (export-pict pict path)
  (-> pict? path-string? any)
  (with-output-to-file path #:exists 'replace
    (thunk (write-bytes (convert (pict->bitmap pict) 'png-bytes)))))

(define/contract (env env-name w h variables)
  (-> string?
      real?
      real?
      (listof (cons/c symbol? any/c))
      pict?)
  (hc-append
   padding
   (text env-name)
   (lc-superimpose
    (car
     (foldl (match-lambda**
             [(line (cons base dy))
              (cons (pin-over base
                              padding dy
                              line)
                    (+ dy line-spacing))])
            (cons (rectangle w h)
                  (/ (- h (* line-spacing
                             (length variables)))
                     2))
            (map (match-lambda
                   [(cons name val)
                    (cond
                      [(pict? val)
                       (vl-append (text (~a name ":") 'modern) val)]
                      [(procedure? val)
                       (val (text (~a name ":") 'modern))]
                      [else
                       (text (format "~a:~s" name val) 'modern)])])
                 variables))))))
