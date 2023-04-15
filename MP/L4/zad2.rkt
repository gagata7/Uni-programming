#lang racket

(define-struct leaf () #:transparent)
(define-struct node (l elem r) #:transparent)

(define tree
  (node (node (leaf) 2 (leaf))
   5
  (node (node (leaf) 6 (leaf))
          8
          (node (leaf) 9 (leaf)))))

(define (fold-tree f x t)
    (cond [(leaf? t) x]
          [(node? t) (f
            (fold-tree f x (node-l t))
            (node-elem t)
            (fold-tree f x (node-r t)))
          ]
    )
)

(define (tree-sum t) (fold-tree + 0 t))

(define (tree-flip t)
  (fold-tree (lambda (left this right)
      (if (leaf? this) this
          (make-node right this left))
  ) (leaf) t)
)

(define (tree-height t)
  (fold-tree (lambda (left this right)
      (if (leaf? this) 0
          (+ 1 (max left right))
      )
  ) 0 t)
)

(define (tree-span t)
  (if (leaf? t) (cons '() '())
     (cons (fold-tree min +inf.0 t) (fold-tree max -inf.0 t)))
)

(define (flatten t)
  (fold-tree (lambda (left this right)
      (append left (cons this right))) '() t)
)