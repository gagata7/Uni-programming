 #lang racket
(require data/heap)

(require rackunit)
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

  	   	

 
	   	

	   	

 ;DELAYS ------------------- 

  	   	

 (define not-delay 1)

 	   	

 (define and-delay 1)

 	   	

 (define nand-delay 1)

  	   	

 (define or-delay 1)

 	   	

 (define nor-delay 1)

 	   	

 (define xor-delay 2)


 ;STRUKTURY-------------------

 
 (struct action (time proc))

  	   	

 (struct sim ([time #:mutable] [actions #:mutable]) )

  	   	

 (struct wire ([value #:mutable] sim [actions #:mutable]))


 ;OBSŁUGA KOPCA ----------------

  	   	

 (define (cmp a b) (<= (action-time a) (action-time b)) )

 
 (define (call sim action)

 	   	

   (heap-remove-min! (sim-actions sim)) ;usun aktulana akcje

  	   	

   ((action-proc action)) ;wykonaj aktualną akcje

 )	   	

 (define (top sim) (heap-min (sim-actions sim)))

 	   	

 (define (no-actions? sim) (= 0 (heap-count (sim-actions sim))))

 	   	

 ;SIM----------------------------

	   	

 (define (make-sim) (sim 0 (make-heap cmp)))


 (define (sim-wait! sim t)

	   	

       (if (no-actions? sim)

	   	

           (set-sim-time! sim (+ (sim-time sim) t))  ;jak nie ma akcji tylko zmien czas

  	   	

           (if (<= (action-time (top sim)) (+ (sim-time sim) t)) ; wykonanie instrukcji

 	   	

               (begin

 	   	

                 (let ([curr  (top sim)])

 	   	

                   (define remain (- (+ (sim-time sim) t) (action-time curr)))

  	   	

                   (set-sim-time! sim (action-time curr))

 	   	

                   (call sim curr)

 	   	

                   (sim-wait! sim remain) ))

  	   	

                (set-sim-time! sim (+ (sim-time sim) t)) ))); jak pierwsze instrukcja wykona sie po czasie to tylko zmien czas

  	   	

           
	   	

 (define (sim-add-action! sim time proc)

	   	

   (heap-add! (sim-actions sim) (action (+ (sim-time sim) time) proc))

 	   	

   )


 ;WIRE----------------------------

  	   	

 (define (make-wire sim1)

 	   	

   (wire #f sim1 null))

  	

 (define (wire-on-change! wire action)

 	   	

   (set-wire-actions! wire (cons action (wire-actions wire)))

 	   	

   (action)

 	   	

   ) 


 (define (wire-sim-actions wire) (sim-actions (wire-sim wire))); useful in this section

 	   	

 (define (wire-time wire) (sim-time (wire-sim wire)))

 	   	

 

  	   	

 (define (wire-set! wire value)

  	   	

  (if (eq? (wire-value wire) value) (void)

  	   	

       (begin

 	   	

         (set-wire-value! wire value)

 	   	

         (for-each (lambda (action ) (action)) (wire-actions wire))

 
         )

 
       )

 	   	

   )

	   	

 ;GATES-------------------------------

 	   	

 (define (gate-not out in)

   (define not-proc (lambda () (wire-set! out (not (wire-value in)) )) )
 	   	

   (define not-action (lambda ()
   	

                        (sim-add-action! (wire-sim in) not-delay not-proc)))
	   	

   (wire-on-change! in not-action)

 ) 	

 (define (gate-and out in1 in2)

  	   	

   (define and-proc (lambda () (wire-set! out (and (wire-value in1) (wire-value in2)))))

	   	

   (define and-action

 	   	

     (lambda () (sim-add-action! (wire-sim in1) and-delay and-proc)))
	   	

   (wire-on-change! in1 and-action)
	   	

   (wire-on-change! in2 and-action)

   )

 (define (gate-or out in1 in2)

 	   	

   (define or-proc (lambda () (wire-set! out (or (wire-value in1) (wire-value in2)))))

	   	

   (define or-action

 	   	

     (lambda () (sim-add-action! (wire-sim in1) or-delay or-proc)))
	   	

   (wire-on-change! in1 or-action)
	   	

   (wire-on-change! in2 or-action)
)	   	

 (define (gate-xor out in1 in2)
 	   	

   (define xor-proc (lambda () (wire-set! out (xor (wire-value in1) (wire-value in2)))))
 	   	

   (define xor-action
	   	

     (lambda () (sim-add-action! (wire-sim in1) xor-delay xor-proc)))

   (wire-on-change! in1 xor-action)

   (wire-on-change! in2 xor-action)
)   	

 (define (gate-nand out in1 in2)
	   	

   (define nand-proc (lambda () (wire-set! out (not (and (wire-value in1) (wire-value in2))))))

   (define nand-action

     (lambda () (sim-add-action! (wire-sim in1) nand-delay nand-proc)))
	   	

   (wire-on-change! in1 nand-action)

   (wire-on-change! in2 nand-action)

   )
	   	

 (define (gate-nor out in1 in2 )

 	   	

   (define nor-proc (lambda () (wire-set! out (not (or (wire-value in1) (wire-value in2))))))
   	

   (define nor-action
	   	

     (lambda () (sim-add-action! (wire-sim in1) nor-delay nor-proc)))
	   	

   (wire-on-change! in1 nor-action)
	   	

   (wire-on-change! in2 nor-action)
)	   	

 ;LOGIX WIRES----------------------------------------------------------
 	   	

 (define (wire-not in)
 	   	

   (define out (make-wire (wire-sim in)))
	   	

   (gate-not out in)
 	   	

   out
 	   	

   )	   	

 (define (wire-and in1 in2)
	   	

   (define sim1 (wire-sim in1))

   (define sim2 (wire-sim in2))
	   	

   (if (not (eq? sim1 sim2)) (error "wires in different simulations") null)
	   	

   (define out (make-wire sim1))

   (gate-and out in1 in2)
 	   	

   out
)	   	

 (define (wire-or in1 in2)

	   	

   (define sim1 (wire-sim in1))

  	   	

   (define sim2 (wire-sim in2))

  	   	

   (if (not (eq? sim1 sim2)) (error "wires in different simulations") null)

	   	

   (define out (make-wire sim1))

  	   	

   (gate-or out in1 in2)

 	   	

   out

 )	   	

 (define (wire-xor in1 in2)

 	   	

   (define sim1 (wire-sim in1))

 	   	

   (define sim2 (wire-sim in2))

  	   	

   (if (not (eq? sim1 sim2)) (error "wires in different simulations") null)

 	   	

   (define out (make-wire sim1))

 	   	

   (gate-xor out in1 in2)

 	   	

   out

)	   	

 (define (wire-nand in1 in2)

 	   	

   (define sim1 (wire-sim in1))

 	   	

   (define sim2 (wire-sim in2))

 	   	

   (if (not (eq? sim1 sim2)) (error "wires in different simulations") null)

 	   	

   (define out (make-wire sim1))

 	   	

   (gate-nand out in1 in2)

out)	   	

 

	   	

 (define (wire-nor in1 in2)

	   	

   (define sim1 (wire-sim in1))
	   	

   (define sim2 (wire-sim in2))

	   	

   (if (not (eq? sim1 sim2)) (error "wires in different simulations") null)

	   	

   (define out (make-wire sim1))

	   	

   (gate-nor out in1 in2)

out)	   	

 ;===========================================================

	   	

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
   	

 (define (flip-flop out clk data)

 	   	

   (define sim (wire-sim data))

 	   	

   (define w1  (make-wire sim))

	   	

   (define w2  (make-wire sim))

 	   	

   (define w3  (wire-nand (wire-and w1 clk) w2))

 	   	

   (gate-nand w1 clk (wire-nand w2 w1))

	   	

   (gate-nand w2 w3 data)

	   	

   (gate-nand out w1 (wire-nand out w3)))
