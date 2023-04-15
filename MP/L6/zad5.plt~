#lang plait

(define-type (NNF 'v)
  (nnf-lit [polarity : Boolean] [var : 'v])
  (nnf-conj [l : (NNF 'v)] [r : (NNF 'v)])
  (nnf-disj [l : (NNF 'v)] [r : (NNF 'v)])
)

(define (neg-nnf nnf)
    (cond 
        [(nnf-lit? nnf) (nnf-lit (not (nnf-lit-polarity nnf)) (nnf-lit-var nnf))]
        [(nnf-conj? nnf) (nnf-disj (neg-nnf (nnf-conj-l nnf)) (neg-nnf (nnf-conj-r nnf)))]
        [(nnf-disj? nnf) (nnf-conj (neg-nnf (nnf-disj-l nnf)) (neg-nnf (nnf-disj-r nnf)))]
    )
)

;np dla formuły w nnf = (nnf-disj (nnf-lit #f 'p) (nnf-lit #f 'q))
;neg-nnf zwraca: (nnf-conj (nnf-lit #t 'p) (nnf-lit #t 'q))