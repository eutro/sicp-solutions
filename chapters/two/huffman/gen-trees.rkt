#lang racket

(require pict
         pict/tree-layout
         file/convertible)

(define (export-pict pic name)
  (let* ([file (build-path "images"
                           (string-append name ".svg"))]
         [port (open-output-file #:mode 'text
                                 #:exists 'replace
                                 file)])
    (write-bytes (convert pic 'svg-bytes) port)
    (close-output-port port)))

(define (leaf? node)
  (eq? (car node)
       'leaf))

(define left-branch car)
(define right-branch cadr)
(define leaf-symbol cadr)
(define node-symbols caddr)

(define (weight node)
  ((if (leaf? node)
        caddr
        cadddr)
   node))

(define (huffman->tree-layout node)
  (cond [(leaf? node)
         (tree-layout #:pict
                      (text (string-append (symbol->string (leaf-symbol node))
                                           " "
                                           (number->string (weight node)))
                            'modern))]
        [(tree-layout #:pict
                      (text (string-append "{"
                                           (string-join
                                            (map symbol->string (node-symbols node)))
                                           "} "
                                           (number->string (weight node)))
                            'modern)
                      (huffman->tree-layout (left-branch node))
                      (huffman->tree-layout (right-branch node)))]))

(define (huffman->pict tree)
  (naive-layered #:x-spacing 16
                 (huffman->tree-layout tree)))

(define n=5-tree
  '(((((leaf A 1)
       (leaf B 2)
       (A B)
       3)
      (leaf C 4)
      (A B C)
      7)
     (leaf D 8)
     (A B C D)
     15)
    (leaf E 16)
    (A B C D E)
    31))

(define n=10-tree
  '((((((((((leaf A 1)
            (leaf B 2)
            (A B)
            3)
           (leaf C 4)
           (A B C) 7)
          (leaf D 8)
          (A B C D) 15)
         (leaf E 16)
         (A B C D E)
         31)
        (leaf F 32)
        (A B C D E F)
        63)
       (leaf G 64)
       (A B C D E F G)
       127)
      (leaf H 128)
      (A B C D E F G H)
      255)
     (leaf I 256)
     (A B C D E F G H I)
     511)
    (leaf J 512)
    (A B C D E F G H I J)
    1023))

(export-pict (huffman->pict n=5-tree)
             "tree-n-5")

(export-pict (huffman->pict n=10-tree)
             "tree-n-10")
