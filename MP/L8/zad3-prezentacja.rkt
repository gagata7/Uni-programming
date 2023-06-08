#lang racket

(require "zad3.rkt")

;-----------------PREZENTACJA-----------------------------------------------------------;
(define x (make-deque))
x
(deque-push-front x 2)
(deque-push-front x 3)
(deque-push-back x 7)
(deque-pop-front x) ; 3
(deque-pop-front x) ; 2
(deque-pop-back x) ; 7
(deque-empty? x) ; #t

(deque-push-front x 21)
(deque-push-front x 37)
(deque-pop-back x) ; 21
(deque-pop-front x) ;37
(deque-pop-back x) ;contract violation bo chce zrobic pop na pustej kolejce
