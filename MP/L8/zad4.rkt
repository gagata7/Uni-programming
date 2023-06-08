#lang plait

(define-type Op
  (op-add) (op-mul) (op-sub) (op-div))

(define-type Exp
  (exp-number [n : Number])
  (exp-op [op : Op] [e1 : Exp] [e2 : Exp]))

(define (parse-Op s)
  (let ([sym (s-exp->symbol s)])
  (cond
    [(equal? sym '+) (op-add)]
    [(equal? sym '-) (op-sub)]
    [(equal? sym '*) (op-mul)]
    [(equal? sym '/) (op-div)])))

(define (parse-Exp s)
  (cond
    [(s-exp-number? s) (exp-number (s-exp->number s))]
    [(s-exp-list? s)
     (let ([xs (s-exp->list s)])
       (exp-op (parse-Op  (first  xs))
               (parse-Exp (second xs))
               (parse-Exp (third  xs))))]))

; ==============================================

(define (eval-op op)
  (type-case Op op
    [(op-add) +]
    [(op-sub) -]
    [(op-mul) *]
    [(op-div) /]))

(define (eval e)
  (type-case Exp e
    [(exp-number n) n]
    [(exp-op op e1 e2)
     ((eval-op op) (eval e1) (eval e2))]))

(define (eval-Exp x) ;x jest typu s-expression (s-exp)
  (cond
    [(s-exp-number? x) (s-exp->number x)] ;jesli s-exp jest liczbą, to zwroc 
    [(s-exp-list? x) ;jesli s-exp jest listą 
     (let ([xs (s-exp->list x)]) ;xs to jest od teraz moja lista
       ((eval-op (parse-Op (first xs))) (eval-Exp (second xs)) (eval-Exp (third xs)))
       ;tu są 3 rzeczy: operator, s-exp i s-exp
       ;wszystkie 3 zostaną od razu zewaluowane
     )
    ]
  )
)

(eval-Exp `(+ 1 2))

