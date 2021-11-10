#lang racket

(require "distance-metrics.rkt")
(require "dataframe-adt.rkt")
(require br/cond)

(define (sample df k) ; choosing k random points from dataset for initial centroids
  (define L (no-of-records df))
  (define data (car df))
  (define list-of-refs '())
  (define samples '())
  
  (for ((i (in-range 0 k)))
    (define idx (random 0 L))
    (while (not (eq? (member idx list-of-refs) #f)) ; if random fn generates same number
           (set! idx (random 0 L))
        )
    (set! list-of-refs (append list-of-refs (list idx)))
    )
  
  (for ([r list-of-refs])
    (set! samples (append samples (list (list-ref data r)))))

  samples
  )

(define (kmeans df k)
  (define centroids (sample df k)) ; initial centroids
  
  centroids)

(define df (make-dataframe "penguins_data.csv"))
(kmeans df 3)