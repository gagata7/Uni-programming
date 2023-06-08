#lang plait

(module+ test
  (print-only-errors #t))

;; abstract syntax -------------------------------

(define-type Op
  (add)
  (sub)
  (mul)
  (div))

(define-type Exp
  (numE [n : Number])
  (opE [op : Op] [args : (Listof Exp)])
) ;operator przyjmuje od teraz LISTĘ argumentów
  ;bo lista moze miec dowolną liczbę elementów

;; parse ----------------------------------------

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    ;SYMBOL - to jakiś operator a ANY to dowolne wyrażenie typu Exp
    [(s-exp-match? `{SYMBOL ANY ...} s) ;ANY może występować dowolnie wiele razy (...)
     (opE (parse-op (s-exp->symbol (first (s-exp->list s))))
          (map parse (rest (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-op [op : Symbol]) : Op
  (cond
    [(eq? op '+) (add)]
    [(eq? op '-) (sub)]
    [(eq? op '*) (mul)]
    [(eq? op '/) (div)]
    [else (error 'parse "unknown operator")]))
                 
(module+ test
  (test (parse `2)
       (numE 2))
  (test (parse `{+ 2 1})
       (opE (add) (list (numE 2) (numE 1))))
  (test (parse `{* 3 4})
       (opE (mul) (list (numE 3) (numE 4))))
  (test (parse `{+ {* 3 4} 8})
       (opE (add)
           (list (opE (mul) (list (numE 3) (numE 4))) (numE 8))))
  (test/exn (parse `{{+ 1 2}})
           "invalid input")
  ;(test/exn (parse `{+ 1})
  ;        "invalid input") ;ten test jest bez sensu bo od teraz + moze przyjmować 1 argument
  (test/exn (parse `{^ 1 2})
           "unknown operator"))
  
;; eval --------------------------------------

(define-type-alias Value Number)

(define (do-ope op el)
  (lambda (xs)
  (cond
               [(empty? xs) el]
               [(empty? (rest xs)) (op el (first xs))]
               [else  (foldl (lambda (x y) (op y x)) (first xs) (rest xs))]
  ))
)

(define (op->proc [op : Op]) : ((Listof Value) -> Value)
  ;funkcja op->proc przyjmuje operator jako argument, i zwraca funkcję która przyjmuje
  ;LISTĘ argumentów i zwraca wartość 
  (type-case Op op
    [(add) (do-ope + 0)]
    [(sub) (do-ope - 0)]
    [(mul) (do-ope * 1)]
    [(div) (do-ope / 1)]
  )
)

;(define (op->proc [op : Op]) : ((Listof Value) -> Value)
;  ;funkcja op->proc przyjmuje operator jako argument, i zwraca funkcję która przyjmuje
;  ;LISTĘ argumentów i zwraca wartość 
;  (type-case Op op
;    [(add) (lambda (xs) (+ (first xs) (second xs)))]
;    [(sub) (lambda (xs) (- (first xs) (second xs)))]
;    [(mul) (lambda (xs) (* (first xs) (second xs)))]
;    [(div) (lambda (xs) (/ (first xs) (second xs)))]
;  )
;)

(define (eval [e : Exp]) : Value
  (type-case Exp e
    [(numE n) n] ;jeśli wyrazenie e jest typu liczbowego (n) to zwróc liczbę
    ;a jeśli wyrażenie jest typu {operator, argumenty} to zwroc operator i z ewaluuj elementy z listy argumentów
    [(opE o args) ((op->proc o) (map eval args))]))


(define (run [e : S-Exp]) : Value
  (eval (parse e)))

(module+ test
  (test (run `{+})
        0)
  (test (run `{+ 2})
        2) 
  (test (run `{* 2 2 2})
        8)
  (test (run `{/ 1 2 3})
        1/6))

;; printer ———————————————————————————————————-

(define (print-value [v : Value]) : Void
  (display v))

(define (main [e : S-Exp]) : Void
  (print-value (eval (parse e))))