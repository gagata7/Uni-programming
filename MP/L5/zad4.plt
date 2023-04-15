#lang plait

(define (perms lst)
  (if (empty? (rest lst))
      (list lst)
      (append-map (lambda (elem)
                    (map (lambda (perm)
                           (cons elem perm))
                         (perms (remove elem lst))))
                  lst)))