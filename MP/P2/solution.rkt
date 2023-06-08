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

(define komparator (lambda (a1 a2) (<= (action-time a1) (action-time a2))))

;Symulacja --------------------------------------------------------------------------

(struct action (time proc));akcja ma czas wykonania i procedurę do zrobienia

(struct sim (actions t) #:mutable #:transparent)
;actions - przechowuje akcje w symulacji, t - to obecny czas symulacji

(define (make-sim) (sim (make-heap komparator) 0))

(define (sim-time sim) (sim-t sim))

(define (sim-do! sim action)
  (heap-remove-min! (sim-actions sim)) ;wywala najwcześniejszą akcję z heapa
  ((action-proc action)) ;wywołuje tą akcję
)

(define (sim-wait! sim delta) ;aktualizuje czas symulacji o delta-jednostek wykonując akcje z heapa
  (if (= 0 (heap-count (sim-actions sim)))
      (set-sim-t! sim (+ (sim-time sim) delta))
      ;jeśli nie ma akcji do wykonania to po prostu skipnij czas symulacji
      (if (<= (action-time (heap-min (sim-actions sim))) (+ (sim-time sim) delta))
          ;jeśli jest akcja do wykonania w tej chwili to:
          (begin
            (let [(current (heap-min (sim-actions sim)))]
              (define remain (- (+ (sim-time sim) delta) (action-time current)));tyle sekund jeszcze ma się wykonywać symulacja
              (set-sim-t! sim (action-time current)) ;ustaw czas symulacji na czas akcji, którą wykonuje
              (sim-do! sim current);wykonaj tą akcję (i wywal z heapa)
              (sim-wait! sim remain);wywołaj funkcję ponownie wykonując kolejne akcje z heapa
            )
          )
          (set-sim-t! sim (+ (sim-time sim) delta))
          ;jeśli czas tej akcji jest > niż aktualny, to jeszcze nie jest pora na jej wykonanie - więc skip
      )
  )
)

(define (sim-add-action! sim time proc)
  (heap-add! (sim-actions sim) (action (+ (sim-time sim) time) proc)))
;tworzy akcję o konkretnym czasie wykonania i procedurze i wkłada ją na stertę


;Przewody ---------------------------------------------------------------------------

(struct wire (sim val actions) #:mutable #:transparent)
;val - to wart. logiczna kabelka, actions to funkcje które są wywoływane - gdy wartość kabelka się zmieni

(define (make-wire sim) (wire sim #f '()))

(define (wire-on-change! wire action)
  (set-wire-actions! wire (cons action (wire-actions wire))) ;dodaje na stos akcji do wykonania dla danego kabelka nową akcję
  (action)
);jeśli kabelek wire zmieni wartość to wykonaj akcję 'action'

(define (wire-value wire) (wire-val wire))

(define (wire-set! wire value)
  (when (not (eq? value (wire-value wire))) ;jesli zmieniam na cos innego co bylo wczesniej
    (set-wire-val! wire value) ;ustaw taka wartosc kabelkowi
    (for-each (lambda (action) (action)) (wire-actions wire))
    ;wykonaj wszystkie akcje triggerowane przez ten kabelek
  )
)

;Bramki logiczne --------------------------------------------------------------------

(define (gate-not out in)
  (wire-on-change! in (lambda () (sim-add-action! (wire-sim in) 1 (lambda () (wire-set! out (not (wire-value in)))))))
)

(define (gate-and out in1 in2)
  (wire-on-change! in1 (lambda () (sim-add-action! (wire-sim in1) 1 (lambda () (wire-set! out (and (wire-value in1) (wire-value in2)))))))
  (wire-on-change! in2 (lambda () (sim-add-action! (wire-sim in2) 1 (lambda () (wire-set! out (and (wire-value in1) (wire-value in2)))))))
)

(define (gate-nand out in1 in2)
  (wire-on-change! in1 (lambda () (sim-add-action! (wire-sim in1) 1 (lambda () (wire-set! out (not (and (wire-value in1) (wire-value in2))))))))
  (wire-on-change! in2 (lambda () (sim-add-action! (wire-sim in2) 1 (lambda () (wire-set! out (not (and (wire-value in1) (wire-value in2))))))))
)

(define (gate-or out in1 in2)
  (wire-on-change! in1 (lambda () (sim-add-action! (wire-sim in1) 1 (lambda () (wire-set! out (or (wire-value in1) (wire-value in2)))))))
  (wire-on-change! in2 (lambda () (sim-add-action! (wire-sim in2) 1 (lambda () (wire-set! out (or (wire-value in1) (wire-value in2)))))))
)

(define (gate-nor out in1 in2)
  (wire-on-change! in1 (lambda () (sim-add-action! (wire-sim in1) 1 (lambda () (wire-set! out (not (or (wire-value in1) (wire-value in2))))))))
  (wire-on-change! in2 (lambda () (sim-add-action! (wire-sim in2) 1 (lambda () (wire-set! out (not (or (wire-value in1) (wire-value in2))))))))
)

(define (gate-xor out in1 in2)
  (wire-on-change! in1 (lambda () (sim-add-action! (wire-sim in1) 2 (lambda () (wire-set! out (xor (wire-value in1) (wire-value in2)))))))
  (wire-on-change! in2 (lambda () (sim-add-action! (wire-sim in2) 2 (lambda () (wire-set! out (xor (wire-value in1) (wire-value in2)))))))
)


;Tworzenie przewodu z bramką która nim steruje---------------------------------

(define (wire-not in)
  (let [(out (make-wire (wire-sim in)))]
    (gate-not out in) out))

(define (wire-and in1 in2)
  (if (not (eq? (wire-sim in1) (wire-sim in2)))
      (error "different simulations")
      (let [(out (make-wire (wire-sim in1)))]
        (gate-and out in1 in2)
        out)))

(define (wire-nand in1 in2)
  (if (not (eq? (wire-sim in1) (wire-sim in2)))
      (error "different simulations")
      (let [(out (make-wire (wire-sim in1)))]
        (gate-nand out in1 in2)
        out)))

(define (wire-or in1 in2)
  (if (not (eq? (wire-sim in1) (wire-sim in2)))
      (error "different simulations")
      (let [(out (make-wire (wire-sim in1)))]
        (gate-or out in1 in2)
        out)))

(define (wire-nor in1 in2)
  (if (not (eq? (wire-sim in1) (wire-sim in2)))
      (error "different simulations")
      (let [(out (make-wire (wire-sim in1)))]
        (gate-nor out in1 in2)
        out)))

(define (wire-xor in1 in2)
  (if (not (eq? (wire-sim in1) (wire-sim in2)))
      (error "different simulations")
      (let [(out (make-wire (wire-sim in1)))]
        (gate-xor out in1 in2)
        out)))

;;Funkcje, które już były zdefiniowane ------------------------------------------------

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

;przerzutnik
(define (flip-flop out clk data)
  (define sim (wire-sim data))
  (define w1  (make-wire sim))
  (define w2  (make-wire sim))
  (define w3  (wire-nand (wire-and w1 clk) w2))
  (gate-nand w1 clk (wire-nand w2 w1))
  (gate-nand w2 w3 data)
  (gate-nand out w1 (wire-nand out w3)))