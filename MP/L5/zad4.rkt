#lang plait

(define (remove elem lst)
  (cond ((empty? lst) '())
        ((equal? elem (first lst)) (rest lst))
        (else (cons (first lst) (remove elem (rest lst))))))

(define (perms lst)
  (define (perms-rec lst elem)
    (if (empty? lst) lst
      
    )
    (perms-rec lst (first lst))
  )
)