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
  (opE [op : Op] [l : Exp] [r : Exp])
  (ifE [b : Exp] [l : Exp] [r : Exp])
  (varE [x : Symbol])
  (letE [x : Symbol] [e1 : Exp] [e2 : Exp])
  (lamE [x : Symbol] [b : Exp]);funkcja
  (appE [proc : Exp] [arg : Exp])) ;aplikacja

;; parse ----------------------------------------

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `{lambda {SYMBOL} ANY} s)
     (lamE (s-exp->symbol
            (first (s-exp->list 
                    (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY ANY} s)
     (opE (parse-op (s-exp->symbol (first (s-exp->list s))))
          (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s))))]
    [(s-exp-match? `{if ANY ANY ANY} s)
     (ifE (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `SYMBOL s)
     (varE (s-exp->symbol s))]
    [(s-exp-match? `{let SYMBOL ANY ANY} s)
     (letE (s-exp->symbol (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
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
  #;(test/exn (parse `{+ 1})
            "invalid input") ;ten test jest z czasów gdy + był op. binarnym
  ;teraz {+ 1} oznacza, że zmienna + ma wartość 1
  (test/exn (parse `{^ 1 2})
            "unknown operator")
  (test (parse `{let x 1 {+ x 1}})
        (letE 'x (numE 1) (opE (add) (varE 'x) (numE 1)))))

;; eval --------------------------------------

;; values

(define-type Value
  (numV [n : Number])
  (boolV [b : Boolean])
  (funV [x : Symbol] [e : Exp] [env : Env]) 
  (funA [body : ExpA])) 

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

;; environments

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define (extend-env [env : Env] [x : Symbol] [v : Value]) : Env
  (cons (bind x v) env))
(define (lookup-env [n : Symbol] [env : Env]) : Value
  (type-case (Listof Binding) env
    [empty (error 'lookup "unbound variable")]
    [(cons b rst-env) (cond
                        [(eq? n (bind-name b))
                         (bind-val b)]
                        [else (lookup-env n rst-env)])]))

;; primitive operations

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

;; evaluation function

(define (eval [e : Exp] [env : Env]) : Value
  (type-case Exp e
    [(numE n) (numV n)]
    [(opE o l r) ((op->proc o) (eval l env) (eval r env))]
    [(ifE b l r)
     (type-case Value (eval b env)
       [(boolV v)
        (if v (eval l env) (eval r env))]
       [else
        (error 'eval "type error")])]
    [(varE x)
     (lookup-env x env)]
    [(letE x e1 e2)
     (let ([v1 (eval e1 env)])
       (eval e2 (extend-env env x v1)))]
    [(lamE x b)
     (funV x b env)] 
    [(appE e1 e2)
     (apply (eval e1 env) (eval e2 env))]))

(define (apply [v1 : Value] [v2 : Value]) : Value
  (type-case Value v1
    [(funV x b env)
     (eval b (extend-env env x v2))] ;wywołaj funkcję v1 na x = v2 czyli dla f z v1: f(x) = f(v2)
    [else (error 'apply "not a function")]))

(define (run [e : S-Exp]) : Value
  (eval (parse e) mt-env))

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
  (test (run `{let x 1 {+ x 1}})
        (numV 2))
  (test (run `{let x 1 {+ x {let y 2 {* x y}}}})
        (numV 3))
  (test (run `{let x 1
                {+ x {let x {+ x 1}
                       {* x 3}}}})
        (numV 7)))

;; printer ———————————————————————————————————-

(define (value->string [v : Value]) : String
  (type-case Value v
    [(numV n) (to-string n)]
    [(boolV b) (if b "true" "false")]
    [else "#<procedure>"]))

(define (print-value [v : Value]) : Void
  (display (value->string v)))

(define (main [e : S-Exp]) : Void
  (print-value (eval (parse e) mt-env)))

;; lexical addressing —————————————————————————————————

;; target

(define-type ExpA
  (numA [n : Number])
  (opA [op : Op] [l : ExpA] [r : ExpA])
  (ifA [b : ExpA] [l : ExpA] [r : ExpA])
  (varA [n : Number]) ;adres zmiennej (to ile letów/lambd temu została związana)
  (letA [a1 : ExpA] [a2 : ExpA])
  (lamA [body : ExpA]) ;ciało funkcji (w którym są już adresy zmiennych zamiast symboli)
  (appA [proc : ExpA] [arg : ExpA])) ;funkcja do wykonania na jakimś argumencie

;; environments (lists of entities)

(define-type-alias (EnvA 'a) (Listof 'a));na pozycji i-tej trzyma zmienną pod adresem i

(define mt-envA empty)

(define (extend-envA [env : (EnvA 'a)] [x : 'a]) : (EnvA 'a)
  (cons x env))

(define (lookup-envA [n : Number] [env : (EnvA 'a)]) : 'a
  (list-ref env n))

;; evaluation function

;oblicza wyrażenie w którym zamiast symboli są adresy zmiennych
(define (evalA [a : ExpA] [env : (EnvA Value)]) : Value
  (type-case ExpA a
    [(numA n) (numV n)]
    [(opA o l r) ((op->proc o) (evalA l env) (evalA r env))]
    [(ifA b l r)
     (type-case Value (evalA b env)
       [(boolV v)
        (if v (evalA l env) (evalA r env))]
       [else
        (error 'eval "type error")])]
    [(varA n)
     (lookup-envA n env)]
    [(letA e1 e2)
     (let ([v1 (evalA e1 env)])
       (evalA e2 (extend-envA env v1)))]
    [(lamA body)
     (funA body)]
    [(appA proc arg)
     (applyA (evalA proc env) (evalA arg env) env)]))

(define (applyA [f : Value] [arg : Value] [env : (EnvA Value)]) : Value
  (type-case Value f
    ;wylicza f(arg) używając wiązań ze środowiska (w którym są tylko adresy)
    [(funA body) (evalA body (extend-envA env arg))]
    [else (error 'apply "not a function")]))

(define (runA [s : S-Exp]) : Value
  (evalA (translate (parse s) mt-envA) mt-envA))

;; translation function

(define (address-of [x : Symbol] [env : (EnvA Symbol)]) : Number
  ;zwraca adres zmiennej w tym środowisku
  (type-case (EnvA Symbol) env
    [empty
     (error 'address-of "unbound variable")]
    [(cons y rst-env)
     (if (eq? x y)
         0
         (+ 1 (address-of x rst-env)))]))

(define (translate [e : Exp] [env : (EnvA Symbol)]) : ExpA
  ;dostaje sparsowane drzewko i zwraca to wyrażenie ale z podstawionymi adresami zmiennych zamiast symboli
  (type-case Exp e
    [(numE n)
     (numA n)]
    [(opE o l r)
     (opA o (translate l env) (translate r env))]
    [(ifE b l r)
     (ifA (translate b env)
          (translate l env)
          (translate r env))]
    [(varE x)
     (varA (address-of x env))]
    [(letE x e1 e2)
     (letA (translate e1 env)
           (translate e2 (extend-envA env x)))]
    [(lamE x b)
     (lamA (translate b (extend-envA env x)))]
    [(appE f a)
     (appA (translate f env) (translate a env))]))

;;Dlaczego adr. leks. dobrze współgra z domknięciami funkcji?--------------------------------------
;na przykładzie: { lambda { x } { let y {* x x } {+ x y }}}
;-> domknięcie pozwala korzystać w funkcji ze zmiennych które się pojawiły wcześniej
;-> adresowanie leksykalne określa kiedy się pojawił dany x
;Jeśli w wyrażeniu do policzenia pojawia się wiele tych samych symboli ale w różnych kontekstach, to
;dzięki adr. leks. jesteśmy w stanie jednoznacznie określić, którego x potrzebujemy użyć w danym kontekście
;z tego przykładu - adresowanie leksykalne zrobi coś takiego:
;(lamA (letA (opA (mul) (varA 0) (varA 0)) (opA (add) (varA 1) (varA 0))))
;wiązanie z x jest teraz na pozycji 1, a wiązanie z y na pozycji 0 (bo jest najświeższe)

(module+ test
  (test (run `2)
        (runA `2))
  (test (run `{+ 2 1})
        (runA `{+ 2 1}))
  (test (run `{* 2 1})
        (runA `{* 2 1}))
  (test (run `{+ {* 2 3} {+ 5 8}})
        (runA `{+ {* 2 3} {+ 5 8}}))
  (test (run `{= 0 1})
        (runA `{= 0 1}))
  (test (run `{if {= 0 1} {* 3 4} 8})
        (runA `{if {= 0 1} {* 3 4} 8}))
  (test (run `{let x 1 {+ x 1}})
        (runA `{let x 1 {+ x 1}}))
  (test (run `{let x 1 {+ x {let y 2 {* x y}}}})
        (runA `{let x 1 {+ x {let y 2 {* x y}}}}))
  (test (run `{let x 1
                {+ x {let x {+ x 1}
                       {* x 3}}}})
        (runA `{let x 1
                 {+ x {let x {+ x 1}
                        {* x 3}}}}))
  (test (run `{{lambda {x} {+ x 1}} 5})
        (runA `{{lambda {x} {+ x 1}} 5})))