#lang racket
(require data/heap)
(provide sim? wire?
         (contract-out
          [make-sim        (-> sim?)]
          [sim-wait!       (-> sim? positive? void?)]
          [sim-time        (-> sim? real?)]
          [sim-add-action! (-> sim? positive? (-> any/c) void?)]

          [make-wire       (-> sim? wire?)]
          [wire-on-change! (-> wire? (-> any/c) void?)]
          [wire-value      (-> wire? boolean?)]
          [wire-set!       (-> wire? boolean? void?)]

          [bus-value (-> (listof wire?) natural?)]
          [bus-set!  (-> (listof wire?) natural? void?)]

          [gate-not  (-> wire? wire? void?)]
          [gate-and  (-> wire? wire? wire? void?)]
          [gate-nand (-> wire? wire? wire? void?)]
          [gate-or   (-> wire? wire? wire? void?)]
          [gate-nor  (-> wire? wire? wire? void?)]
          [gate-xor  (-> wire? wire? wire? void?)]

          [wire-not  (-> wire? wire?)]
          [wire-and  (-> wire? wire? wire?)]
          [wire-nand (-> wire? wire? wire?)]
          [wire-or   (-> wire? wire? wire?)]
          [wire-nor  (-> wire? wire? wire?)]
          [wire-xor  (-> wire? wire? wire?)]

          [flip-flop (-> wire? wire? wire? void?)]))

;Komparator do heapa ----------------------------------------------------------------

(define komparator (lambda (para1 para2) (<= (car para1) (car para2))))

;Symulacja --------------------------------------------------------------------------

(struct sim (queue t) #:mutable #:transparent)
;queue - przechowuje zdarzenia w symulacji, t - to obecny czas symulacji

(define (make-sim) (sim (make-heap komparator) 0))
;tworzy nową symulację w której na razie nie ma zdarzeń i czas = 0

(define (sim-time sim) (sim-t sim))
;zwraca ile czasu upłynęło w konkretnej symulacji

(define (sim-wait! sim delta) ;aktualizuje czas symulacji o delta-jednostek
  (set-sim-t! sim (+ (sim-time sim) delta)))

(define (sim-add-action! sim delay action)
  (let ((event-time (+ (sim-time sim) delay)))
    (heap-add! (sim-queue sim) (cons event-time (lambda () (action))))))
;wkłada na stertę konkretnej symulacji, parę: akcję i jej czas wykonania


;Przewody ---------------------------------------------------------------------------

(struct wire (sim val observers) #:mutable #:transparent)
;val - to wart. logiczna kabelka, observers to funkcje które są wywoływane
;gdy wartość kabelka się zmieni

(define (make-wire sim) (wire sim #f '()))
;tworzenie nowego kabelka

(define (wire-on-change! wire action)
  (set-wire-observers! wire (cons action (wire-observers wire))))
;podpinam do tego kabelka jakąś akcję

(define (wire-value wire) (wire-val wire));zwraca wartość kabelka

(define (wire-set! wire value)
  (when (not (eq? value (wire-value wire))) ;jesli zmieniam na cos innego co bylo wczesniej
    (set-wire-val! wire value) ;ustaw taka wartosc kabelkowi
    (for-each (lambda (observer) (observer value)) (wire-observers wire))
    ;wykonaj wszystkie podpiete do niego operacje
  )
)

;Bramki logiczne --------------------------------------------------------------------

(define (gate-not out in)        ;nie wiem czy tu nie powinno byc lambda () (wire-set! ...)
  (sim-add-action! (wire-sim out) 1 (wire-set! out (not (wire-value in)))))

(define (gate-and out in1 in2)
  (sim-add-action! (wire-sim out) 1
    (wire-set! out (and (wire-value in1) (wire-value in2)))))

(define (gate-nand out in1 in2)
  (sim-add-action! (wire-sim out) 1
    (wire-set! out (not (and (wire-value in1) (wire-value in2))))))

(define (gate-or out in1 in2)
  (sim-add-action! (wire-sim out) 1
    (wire-set! out (or (wire-value in1) (wire-value in2)))))

(define (gate-nor out in1 in2)
  (sim-add-action! (wire-sim out) 1
    (wire-set! out (not (or (wire-value in1) (wire-value in2))))))

(define (gate-xor out in1 in2)
  (sim-add-action! (wire-sim out) 2
    (wire-set! out (if (equal? (wire-value in1) (wire-value in2)) #f #t))))


;Tworzenie przewodu z bramką która nim steruje-----------------------------

(define (wire-not in)
  (let [(out (make-wire (wire-sim in)))]
    (gate-not out in) out))

(define (wire-and in1 in2)
  (let [(out (make-wire (wire-sim in1)))]
    (gate-and out in1 in2)
    out))

(define (wire-nand in1 in2)
  (let [(out (make-wire (wire-sim in1)))]
    (gate-nand out in1 in2)
    out))

(define (wire-or in1 in2)
  (let [(out (make-wire (wire-sim in1)))]
    (gate-or out in1 in2)
    out))

(define (wire-nor in1 in2)
  (let [(out (make-wire (wire-sim in1)))]
    (gate-nor out in1 in2)
    out))

(define (wire-xor in1 in2)
  (let [(out (make-wire (wire-sim in1)))]
    (gate-xor out in1 in2)
    out))

;;Funkcje, które już były zdefiniowane ----------------------------------------------

(define (bus-set! wires value)
  (match wires
    ['() (void)]
    [(cons w wires)
     (begin
       (wire-set! w (= (modulo value 2) 1))
       (bus-set! wires (quotient value 2)))]))

(define (bus-value ws)
  (foldr (lambda (w value) (+ (if (wire-value w) 1 0) (* 2 value)))
         0
         ws))

;; Flip-flop (przerzutnik)
(define (flip-flop out clk data)
(define sim (wire-sim data))
(define w1 (make-wire sim))
(define w2 (make-wire sim))
(define w3 (wire-nand (wire-and w1 clk) w2))
(gate-nand w1 clk (wire-nand w2 w1))
(gate-nand w2 w3 data)
(gate-nand out w1 (wire-nand out w3)))
