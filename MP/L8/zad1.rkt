#lang racket

(define (cycle-rec! xs x) 
   (if (eq? (mcdr xs) null) (set-mcdr! xs x)
      (cycle-rec! (mcdr xs) x)
   )
)

(define (cycle! x)
  (if (null? x) '()
    (cycle-rec! x x)
  )
)

(define x (mcons 1 (mcons 2 (mcons 3 (mcons 4 '())))))
(define y (mcons 1 null))
(cycle! y)
(cycle! x)
y
x
;#0=(mcons 1 (mcons 2 (mcons 3 (mcons 4 #0#))))
; #0= znaczy początek wyrażenia, które jest współdzielone
; #0# to taki koniec - który odnosi się do całego wyrażenia
; #0# umieszczone na końcu - oznacza, że ostatni element listy konczy się całą listą x