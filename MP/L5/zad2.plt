#lang plait

( define ( apply f x ) ( f x ) )
;typ apply: (('a -> 'b) 'a -> 'b)

;funkcja apply jako argument1 przyjmuje funkcję, która bierze jakiś x o typie
;'a i zwraca coś o typie 'b (nie wiadomo czym to jest)
;oraz jako argument2 bierze x o typie 'a
; funkcja apply zwraca po prostu wartosc typu 'b - czyli
;wynik wywołania funkcji f dla arg. x
;zatem apply bedzie typu: ('a -> 'b) 'a -> 'b

( define ( compose f g ) ( lambda ( x ) ( f ( g x ) ) ) )
;typ compose: (('a -> 'b) ('c -> 'a) -> ('c -> 'b))

;funkcja g przyjmuje arg typu 'c i zwraca typu 'a
;funkcja f bierze ten arg typu 'a i zwraca cos typu 'b
; czyli f(g(x)) bierze cos typu 'c i zwraca coś typu 'b

( define ( flip f ) ( lambda ( x y ) ( f y x ) ) )
;typ flip: (('a 'b -> 'c) -> ('b 'a -> 'c)) ^
;funkcja f bierze 2 argumenty --------------|
;x oraz y, które są typów odpowiednio: 'a i 'b
;funkcja f zwraca cos typu 'c

;flip bierze tą funkcję f jako argument czyli przyjmuje za argument:
;('a 'b -> 'c) a zwraca też funkcję f ale na y x czyli są zamienione
;i zwraca cos typu 'c wiec: ('b 'a -> 'c)

( define ( curry f ) ( lambda ( x ) ( lambda ( y ) ( f x y ) ) ) ) ;to jest częściowa aplikacja
;typ curry: (('a 'b -> 'c) -> ('a -> ('b -> 'c)))

;ponownie curry przyjmuje za argument funkcję f, która bierze 2 argumenty
;x typu 'a i y typu 'b i zwraca coś typu 'c czyli przyjmuje
;f typu: ('a 'b -> 'c)
;najpierw się wykona ta wewnetrzna lambda: ( lambda ( y ) ( f x y ) )
;która bierze y typu 'b i zwraca to co f zwraca czyli 'c
; potem wywoła zewnetrzną lambdę dla x typu 'a na funkcji ('b -> 'c)
;zatem zwróci ('a -> ('b -> 'c))