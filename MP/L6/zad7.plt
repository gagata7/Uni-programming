#lang plait

(define-type (Formula 'v)
  (var [var : 'v ])
  (neg [f : (Formula 'v)])
  (conj [l : (Formula 'v)] [r : (Formula 'v)])
  (disj [l : (Formula 'v)] [r : (Formula 'v)])
  )

(define (eval-formula s f)
  (cond
    [(var? f) (s var-var f)]
    [(neg? f) (not (eval-formula s (neg-f f)))]
    [(conj? f) (and (eval-formula s (conj-l f)) (eval-formula s (conj-r f)))]
    [(disj? f) (or (eval-formula s (disj-l f)) (eval-formula s (disj-r f)))]
    )
  )