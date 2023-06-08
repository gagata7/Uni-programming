#lang racket
(provide ;provide udostępnia moje już zaimplementowane funkcje do innych programów
 ;mogę ich potem użyć, dodając w innym programie 'require'
 deque?
 nonempty-deque?
 (contract-out
  [deque-empty? (-> deque? boolean?)]
  [make-deque (-> deque?)]
  [deque-push-front (-> deque? any/c void?)]
  [deque-push-back (-> deque? any/c void?)]
  [deque-pop-front (-> deque? any/c)]
  [deque-pop-back (-> deque? any/c)]
 )
)

(struct deque ;deque tak jak queue ma początek i koniec
  ([front #:mutable]
   [back #:mutable]
  )
)

(struct deque-node ;atrybuty węzła deque
  ([left #:mutable] ;wskaznik na lewy element
   [val #:mutable] ;wartość w węźle
   [right #:mutable] ;wskaźnik na prawy element
  )
)

(define (deque-empty? q) ;deque też może być pusta i trzeba to sprawdzić
  (and (null? (deque-front q))
       (null? (deque-back  q))))

(define (nonempty-deque? q); analogicznie sprawdzanie czy nie pusta dla deque
  (and (deque? q) (deque-node? (deque-front q))))

(define (make-deque) ;tworzenie deque
  (deque null null)
)

(define (deque-push-front q x)
  (if (deque-empty? q) ;jesli kolejka jest pusta
      (let [(nowy (deque-node null x null))] ;to stworz nowy węzeł o wartosci x
        ;ktory będzie pierwszym i jedynym węzłem tej kolejki
        (set-deque-front! q nowy) ;więc będzie na początku
        (set-deque-back! q nowy) ;i na końcu
      )
      ;jeśli kolejka nie jest pusta
      (let [(stary (deque-front q)) (nowy (deque-node null x null))]
        (set-deque-node-right! nowy stary)
        (set-deque-node-left! stary nowy)
        (set-deque-front! q nowy)
      )
  )
)

(define (deque-push-back q x)
  (if (deque-empty? q) ;jesli kolejka jest pusta
      (let [(nowy (deque-node null x null))] ;to stworz nowy węzeł o wartosci x
        ;ktory będzie pierwszym i jedynym węzłem tej kolejki
        (set-deque-front! q nowy) ;więc będzie na początku
        (set-deque-back! q nowy) ;i na końcu
      )
      ;jeśli kolejka nie jest pusta
      (let [(stary (deque-back q)) (nowy (deque-node null x null))]
        (set-deque-node-right! stary nowy)
        (set-deque-node-left! nowy stary)
        (set-deque-back! q nowy)
      )
  )
)
(define/contract (deque-pop-front q) ;tu kontrakt bo q nie moze byc pusta
  (-> nonempty-deque? void)
  (define pierwszy (deque-front q))
  (if (null? (deque-node-right pierwszy))
      (begin (set-deque-front! q null)
             (set-deque-back! q null)
             (deque-node-val pierwszy))
      (let [(drugi (deque-node-right pierwszy))]
        (begin (set-deque-front! q drugi)
               (set-deque-node-left! drugi null)
               (deque-node-val pierwszy))
      )
  )
)
(define (deque-pop-back q) ;tu tez kontrakt ten sam
  (-> nonempty-deque? void)
  (define ostatni (deque-back q))
  (if (null? (deque-node-left ostatni))
      (begin (set-deque-front! q null)
             (set-deque-back! q null)
             (deque-node-val ostatni))
      (let [(drugi (deque-node-left ostatni))]
        (begin (set-deque-back! q drugi)
               (set-deque-node-right! drugi null)
               (deque-node-val ostatni))
      )
  )
)


