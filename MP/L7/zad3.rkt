#lang racket

(define (suffixes1 xs)
  (match xs 
    ['() (list null)]
    [xs (cons xs (suffixes1 (cdr xs)))]
  )
)

(define/contract (suffixes2 xs)
  (parametric->/c [a] (-> (listof a) (listof (listof a))))
  ;elementy listy xs są wrzucane do wora 'a'
  ;suffixes2 przyjmuje (listof a) czyli listę, której elementy są w worku 'a'
  ;suffixes2 zwraca (listof (listof a)) czyli wielką listę mniejszych list, w której mniejsze listy składają się z elementów z worka 'a'
  (match xs
    ['() (list null)]
    [xs (cons xs (suffixes2 (cdr xs)))]
  )
)

(define x (time (suffixes1 (range 5000))));bez kontraktu
(define y (time (suffixes2 (range 5000))));z kontraktem
;(define cos (time (f xs))) -> cos zwraca to co wypisuje time - czyli czas, a nie to co wypisuje (f xs)

