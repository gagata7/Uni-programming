#lang plait

;('a 'b - > 'a )
(define (f1 a b) a)

;(('a 'b - > 'c ) ('a - > 'b ) 'a - > 'c )
(define (f2 f g a) (f a (g a)))

;((( 'a - > 'a ) - > 'a ) - > 'a )
(define (f3 f) (f (lambda (x) (f (lambda (x) x)))))
(define (f3_other [x : (('a -> 'a) -> 'a)]) (x (lambda (x) x)))

;(('a - > 'b ) ('a - > 'c ) - > ('a - > ('b * 'c ) ) )
(define (f4 f g) (lambda (x) (pair (f x) (g x))))

;(('a - > ( Optionof ('a * 'b ) ) ) 'a - > ( Listof 'b ) )
(define (wynik a b)
  (if (none? wynik) empty
      (list (snd (some-v (wynik b)) (unfold a (first (some-v wynik)))))
  )
)