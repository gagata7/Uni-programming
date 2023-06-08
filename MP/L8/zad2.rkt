#lang racket
(define (mreverse! xs)
    (define (mreverse-rec left right)
        (if (null? right) left
            (let [(next (mcdr right))]
                (set-mcdr! right left)
                (mreverse-rec right next)
            )
        )
    )
    (mreverse-rec null xs)
)

(define x (mcons 2 (mcons 1 (mcons 3 (mcons 7 '())))))
(set! x (mreverse! x))
x