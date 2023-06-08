#lang plait

(module+ test
  (print-only-errors #t))

;; abstract syntax -------------------------------

(define-type Op
  (add)
  (sub)
  (mul)
  (div)
  (eql)
  (leq))

(define-type Exp
  (numE [n : Number])
  (boolE [b : Boolean]);od teraz Exp moze byc boolem #t lub #f
  (opE [op : Op]
       [l : Exp]
       [r : Exp])
  (ifE [b : Exp]
       [l : Exp]
       [r : Exp])
)

;; parse ----------------------------------------

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `#t s) (boolE #t)] ;jesli wyrazenie jest #t to zwroc boolE #t
    [(s-exp-match? `#f s) (boolE #f)] ;analogicznie dla falszu
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `{if ANY ANY ANY} s)
     (ifE (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY ANY} s)
     (opE (parse-op (s-exp->symbol (first (s-exp->list s))))
          (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-op [op : Symbol]) : Op
  (cond
    [(eq? op '+) (add)]
    [(eq? op '-) (sub)]
    [(eq? op '*) (mul)]
    [(eq? op '/) (div)]
    [(eq? op '=) (eql)]
    [(eq? op '<=) (leq)]
    [else (error 'parse "unknown operator")]))
                
(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `{+ 2 1})
        (opE (add) (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (opE (mul) (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (opE (add)
             (opE (mul) (numE 3) (numE 4))
             (numE 8)))
  (test (parse `{if {= 0 1} {* 3 4} 8})
        (ifE (opE (eql) (numE 0) (numE 1))
             (opE (mul) (numE 3) (numE 4))
             (numE 8)))
   (test/exn (parse `{{+ 1 2}})
            "invalid input")
  (test/exn (parse `{+ 1})
            "invalid input")
  (test/exn (parse `{^ 1 2})
            "unknown operator")
  )
  
;; eval --------------------------------------

(define-type Value
  (numV [n : Number])
  (boolV [b : Boolean]))

(define (op-num-num->proc [f : (Number Number -> Number)]) : (Value Value -> Value)
  (λ (v1 v2)
    (type-case Value v1
      [(numV n1)
       (type-case Value v2
         [(numV n2)
          (numV (f n1 n2))]
         [else
          (error 'eval "type error")])]
      [else
       (error 'eval "type error")])))

(define (op-num-bool->proc [f : (Number Number -> Boolean)]) : (Value Value -> Value)
  (λ (v1 v2)
    (type-case Value v1
      [(numV n1)
       (type-case Value v2
         [(numV n2)
          (boolV (f n1 n2))]
         [else
          (error 'eval "type error")])]
      [else
       (error 'eval "type error")])))

(define (op->proc [op : Op]) : (Value Value -> Value)
  (type-case Op op
    [(add) (op-num-num->proc +)]
    [(sub) (op-num-num->proc -)]
    [(mul) (op-num-num->proc *)]
    [(div) (op-num-num->proc /)]
    [(eql) (op-num-bool->proc =)]
    [(leq) (op-num-bool->proc <=)]))

(define (eval [e : Exp]) : Value
  (type-case Exp e
    [(numE n) (numV n)]
    [(opE o l r) ((op->proc o) (eval l) (eval r))]
    [(boolE b) (boolV b)]
    [(ifE b l r)
     (type-case Value (eval b)
       [(boolV v)
        (if v (eval l) (eval r))]
       [else
        (error 'eval "type error")])]
    ))

(define (cond->if [cs : (Listof (Exp * Exp))]) : Exp
  (type-case (Listof (Exp * Exp)) cs
    [empty
     (numE 42)]
    [(cons c cs)
     (ifE (fst c)
          (snd c )
          (cond->if cs))]))

(define (run [e : S-Exp]) : Value
  (eval (parse e)))

(module+ test
  (test (run `2)
        (numV 2))
  (test (run `{+ 2 1})
        (numV 3))
  (test (run `{* 2 1})
        (numV 2))
  (test (run `{+ {* 2 3} {+ 5 8}})
        (numV 19))
  (test (run `{= 0 1})
        (boolV #f))
  (test (run `{if {= 0 1} {* 3 4} 8})
        (numV 8))
  )

;; printer ———————————————————————————————————-

(define (value->string [v : Value]) : String
  (type-case Value v
    [(numV n) (to-string n)]
    [(boolV b) (if b "true" "false")]))

(define (print-value [v : Value]) : Void
  (display (value->string v)))

(define (main [e : S-Exp]) : Void
  (print-value (eval (parse e))))

;;MASZYNA ABSTRAKCYJNA DLA BOOLI I NIE TYLKO-----------------------------------
(define-type Stack
  (emptyS)
  (ifS [l : Exp] [r : Exp] [s : Stack])
  ;jak mam ifa to musze pamietac co bylo kiedy warunek=true i warunek=false
  (rightS [op : Op] [exp : Exp] [s : Stack]) ;pamietam prawą rzecz (bo poszłam w lewo)
  (leftS [op : Op] [val : Value] [s : Stack])) ;pamiętam lewą rzecz (bo poszłam w prawo)

(define (evalAM [e : Exp] [s : Stack]) : Value
  (type-case Exp e
    [(numE n)
     (continueAM s (numV n))]
    [(boolE b) (continueAM s (boolV b))]
    ;jak mam boola w łapie, to idź w górę drzewa z tym boolem i obecnym stosem
    [(ifE b l r) (evalAM b (ifS l r s))]
    ;jak mam ifa to idz w dół z tym warunkiem b do sprawdzenia i wrzuc na stos tego ifa
    ;wraz z l i r zeby pamietac co zwrocic (podczas cofania coninue) po rozpatrzeniu
    ;tego warunku b
    [(opE o e1 e2)
     (evalAM e1 (rightS o e2 s))]))

(define (continueAM [s : Stack] [v : Value]) : Value
  (type-case Stack s
    [(emptyS) v]
    [(ifS l r s)
     (if (if (numV? v) ;jezeli v (Value) jest liczbą to if jest zły więc zwróc błąd
             (error 'continueAM "nie moze byc liczba")
             (boolV-b v)) (evalAM l s) (evalAM r s))]
    ;jeśli v był boolem, to ok - w zaleznosci czy to #t czy #f idz w lewo/prawo 
    [(rightS op e s) (evalAM e (leftS op v s))]
    [(leftS op u s) (continueAM s ((op->proc op) v u))]))
  
(define (runAM [e : S-Exp]) : Value
  (evalAM (parse e) (emptyS)))

(module+ test
  (test (run `2)
       (runAM `2))
  (test (run `{+ 2 1})
       (runAM `{+ 2 1}))
  (test (run `{* 2 1})
       (runAM `{* 2 1}))
  (test (run `{+ {* 2 3} {+ 5 8}})
       (runAM `{+ {* 2 3} {+ 5 8}}))
  (test (run `{if #t 2 3})
       (runAM `{if #t 2 3}))
  (test (run `{if (= 2 3) 2 3})
       (runAM `{if (= 2 3) 2 3})))