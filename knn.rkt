#lang racket

(require "distance-metrics.rkt")
(require "list-helper-functions.rkt")
(provide (all-defined-out))

(define (sort-by-distance lst)
  (sort lst (lambda (x y) (< (car x) (car y))))
  )

(define (k-nearest-pairs lst k)
  (define (first-n-pairs lst n)
    (if (eq? n 0)
        '()
        (list (car lst)
              (first-n-pairs (cdr lst) (- n 1))
              )
        )
    )
  (define sorted-pairs (sort-by-distance lst))
  (first-n-pairs sorted-pairs k)
  )

(define (knn-classifier k train-x train-y test)
  (define dp '())
  (define labels '())
  (define neighbours '())

  (for ((i (in-range 0 (length test))))
        (set! dp (list-ref test i))
        (define distances (list ))
    
        ;loop to run through elements of train
        (for ((j (in-range 0 (length train-x))))
           (set! distances (append distances (list (cons (euclidean-distance
                                                    dp
                                                    (list-ref train-x j)
                                                    )
                                                   
                                                   (list-ref train-y j)
                                                   )
                                              )
                                   )
                 )
           )
    (set! neighbours (k-nearest-pairs distances k))
    (set! labels (append labels (mode (map cdr neighbours))))
    
    
    )
  labels
  )

(define (accuracy pred truth)
  (define (match pred truth res)
    (if (null? pred)
        res
        (if (equal? (car pred) (caar truth))
            (match (cdr pred) (cdr truth) (+ res 1))
            (match (cdr pred) (cdr truth) res)
            )
        )
  )
  (define res (match pred truth 0))
  (* (/ res (length pred)) 100.0)
)
