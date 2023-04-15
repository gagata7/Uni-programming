#lang plait

;apply jest typu (('a -> 'b) 'a -> 'b)
( define ( apply f x ) ( f x ) )

;compose jest typu (('a -> 'b) ('c -> 'a) -> ('c -> 'b))
( define ( compose f g ) ( lambda ( x ) ( f ( g x ) ) ) )

;flip jest typu (('a 'b -> 'c) -> ('b 'a -> 'c))
( define ( flip f ) ( lambda ( x y ) ( f y x ) ) )

;curry jest typu (('a 'b -> 'c) -> ('a -> ('b -> 'c)))
( define ( curry f ) ( lambda ( x ) ( lambda ( y ) ( f x y ) ) ) )

;(curry compose)
;curry ma typ (('a 'b -> 'c) -> ('a -> ('b -> 'c)))
;compose ma typ (('x -> 'y) ('z -> 'x) -> ('z -> 'y))
;zatem
;funkcja curry dostanie jako argument funkcję compose
;czyli 'a = ('x -> 'y), 'b = ('z -> 'x), 'c = ('z -> 'y)
;i zwróci 'a -> ('b -> 'c) czyli jak podstawimy spowrotem za 'a, 'b i 'c to:
;('x -> 'y) -> (('z -> 'x) -> ('z -> 'y))
;a to się zgadza bo plait zwróci:
;(('_a -> '_b) -> (('_c -> '_a) -> ('_c -> '_b)))

;((curry compose) (curry compose))
;(curry compose) ma typ: ('x -> 'y) -> (('z -> 'x) -> ('z -> 'y))
; i chcę to podstawic do (curry compose) więc:
;pierwszy argument ma typ: ('a -> 'b) więc a = ('x -> 'y) oraz
; b = (('z -> 'x) -> ('z -> 'y))
;zwracany typ to: (('c -> 'a) -> ('c -> 'b))) więc podstawiam za a i b:
;('c -> ('x -> 'y)) -> ('c ->(('z -> 'x) -> ('z -> 'y)) )
; i to się zgadza, bo racket zwraca:
;(('_a -> ('_b -> '_c)) -> ('_a -> (('_d -> '_b) -> ('_d -> '_c))))

;((curry compose) (curry apply))
;(curry compose) ma typ (('a -> 'b) -> (('c -> 'a) -> ('c -> 'b)))
;(curry apply) ma typ (('x -> 'y) -> ('x -> 'y))
;w (curry compose) 'a = ('x -> 'y) i 'b = ('x -> 'y)
;wiec podstawiajac odpowiednio za 'a i 'b, to co zwraca (curry compose) to:
;(('c -> ('x -> 'y)) -> ('c -> ('x -> 'y))) i to się zgadza, bo racket zwraca:
;(('_a -> ('_b -> '_c)) -> ('_a -> ('_b -> '_c)))

;((curry apply) (curry compose))
;(curry compose) ma typ (('a -> 'b) -> (('c -> 'a) -> ('c -> 'b)))
;(curry apply) ma typ (('x -> 'y) -> ('x -> 'y))
;czyli po wsadzeniu do (curry apply) 'x = ('a -> 'b) a 'y = (('c -> 'a) -> ('c -> 'b))
;a zwrócić ma 'x -> 'y czyli (('a -> 'b) -> (('c -> 'a) -> ('c -> 'b)))
;to samo zwraca racket: (('_a -> '_b) -> (('_c -> '_a) -> ('_c -> '_b)))

;(compose curry flip)
;compose dostaje 2 argumenty: curry typu ('a -> 'b) i flip ('c -> 'a)
;zatem dla curry 'a = ('x1 'y1 -> 'z1) 'b = ('x1 -> ('y1 -> 'z1))
;dla flip: 'c = ('x2 'y2 -> 'z2) i 'a = ('y2 'x2 -> 'z2)
;compose zwraca ('c -> 'b) zatem podstawiając odpowiednio za c i b:
;('x2 'y2 -> 'z2) ->('x1 -> ('y1 -> 'z1))
; racket zwraca (('_a '_b -> '_c) -> ('_b -> ('_a -> '_c)))

