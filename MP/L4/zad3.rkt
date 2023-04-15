#lang racket

(define-struct leaf () #:transparent)
(define-struct node (l elem r) #:transparent)

(define t
  (node (node (leaf) 1 (leaf))
        2
        (node (node (leaf) 3 (leaf))
              4
              (leaf))))

(define (bst? t)
  (define (bst-rec t)
    (define bst-lewy (if (leaf? t) '() (bst-rec (node-l t))))
    (define bst-prawy (if (leaf? t) '() (bst-rec (node-r t))))
    (define min-war (if (leaf? t) +inf.0
      (min (second bst-lewy) (node-elem t) (second bst-prawy))
    ))
    (define max-war (if (leaf? t) -inf.0
      (max (third bst-prawy) (node-elem t) (third bst-prawy))
    ))
    (define czy-bst
      (or (leaf? t) (and
        (first bst-lewy) (first bst-prawy)
        (< (node-elem t) (second bst-prawy))
        (> (node-elem t) (third bst-lewy))
      ))
    )
    (list czy-bst min-war max-war)
  )
  (first (bst-rec t))
)

(define (sum-paths t)
    (define (sum-paths-rec t sum)
        (if (leaf? t) (leaf)
            (node
                (sum-paths-rec (node-l t) (+ sum (node-elem t)))
                (+ sum (node-elem t))
                (sum-paths-rec (node-r t) (+ sum (node-elem t)))
            )
        )
    )
    (sum-paths-rec t 0)
)