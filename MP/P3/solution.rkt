#lang plait

(define-type Op
  (add)
  (sub)
  (mul)
  (leq)); dla <=

(define-type Exp
  (numE [n : Number]);stała liczbowa
  (varE [x : Symbol]);zmienna
  (opE [op : Op] [e1 : Exp] [e2 : Exp]); e1 (operator) e2
  (ifE [e0 : Exp] [e1 : Exp] [e2 : Exp]); ifz e0 then e1 else e2
  (letE [x : Symbol] [e1 : Exp] [e2 : Exp]); let x be e1 in e2
  (funE [name : Symbol] [args : (Listof Exp)] [body : Exp])); fun f(x1, ... x_l) = e

;Funkcje pomocnicze----------------------------------------------------
(define (n-th n ls) (if (> n 1) (n-th (- n 1) (rest ls)) (first ls)))
(define (fifth ls) (n-th 5 ls))
(define (sixth ls) (n-th 6 ls))



;Parser--------------------------------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s); dla liczby n
     (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s); dla zmiennej x
     (varE (s-exp->symbol s))]
    [(s-exp-match? `{ANY SYMBOL ANY} s); dla wyrażenia e1 (operator) e2
     (opE (parse-op (s-exp->symbol (second (s-exp->list s))))
          (parse (first (s-exp->list s)))
          (parse (third (s-exp->list s))))]
    [(s-exp-match? `{ifz ANY then ANY else ANY} s); dla wyrażeń if
     (ifE (parse (second (s-exp->list s)))
          (parse (fourth (s-exp->list s)))
          (parse (sixth (s-exp->list s))))]
    [(s-exp-match? `{let SYMBOL be ANY in ANY} s); dla wyrażeń let x be e1 in e2
     (letE (s-exp->symbol (second (s-exp->list s)))
           (parse (fourth (s-exp->list s)))
           (parse (sixth (s-exp->list s))))]
    [(s-exp-match? `{[fun SYMBOL (ANY ...) = ANY]} s) ; for function expressions
     (let* ([function (s-exp->list (first (s-exp->list s)))] 
            [name (s-exp->symbol (second function))]
            [args (map parse (s-exp->list (third function)))]
            [body (parse (fifth (s-exp->list s)))])
       (funE name args body))]
    [else (error 'parse "invalid input")]))

(define (parse-op [op : Symbol]) : Op
  (cond
    [(eq? op '+) (add)]
    [(eq? op '-) (sub)]
    [(eq? op '*) (mul)]
    [(eq? op '<=) (leq)]
    [else (error 'parse "unknown operator")]))

(define-type-alias Value Number)

(define (run [s : S-Exp]) : Value
  (error 'run "not implemented"))
