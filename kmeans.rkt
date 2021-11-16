#lang racket

(require "distance-metrics.rkt")
(require "dataframe-adt.rkt")
(require "list-helper-functions.rkt")
(require br/cond)
(require sdraw)

(define (sample df k) ; choosing k random points from dataset for initial centroids
  (define L (no-of-records df))
  (define data (car df))
  (define list-of-refs '())
  (define samples '())
  
  (for ((i (in-range 0 k)))
    (define idx (random 1 L))
    (while (not (eq? (member idx list-of-refs) #f)) ; if random fn generates same number
           (set! idx (random 1 L))
           )
    (set! list-of-refs (append list-of-refs (list idx)))
    )
  
  (for ([r list-of-refs])
    (set! samples (append samples (list (list-ref data r)))))

  ;(list->vector samples)
  samples
  )

(define (kmeans df k)
  (define centroids (sample df k)) ; initial centroids <vector>
  (define n_iter 100)
  (define data (car df))
  (define clusters '())

  (define (update-clusters)
    (define row '())
    (set! clusters (list->vector (map list centroids)))
    (for ((i (in-range 1 (no-of-records df))))
      (set! row (list-ref data i))
      (define min-dist +inf.0)
      (define min-dist-idx 0)
      
      (for ((j (in-range 0 k))) 
        (define tmp-dist (euclidean-distance row (list-ref centroids j)))
        (if (and (not (= tmp-dist 0)) (< tmp-dist min-dist))
            (begin
              (set! min-dist tmp-dist)
              (set! min-dist-idx j)
              )
            (void)
            )
        ) 

      (vector-set! clusters min-dist-idx (append
                                          (vector-ref clusters min-dist-idx)
                                          (list row)
                                          )
                   )
      )
    )

  (define (update-centroids)
    (define new-centroids '())
    (for ((i (in-range 0 k)))
      (set! new-centroids (append
                           new-centroids
                           (list (centroid (vector-ref clusters i)))
                           )
            )
    )
    (set! centroids new-centroids)
    )

  (for ((i (in-range 0 n_iter)))
    (update-clusters)
    (update-centroids)
    )
  
  (cons clusters centroids) 
  )

(define df (make-dataframe "Mall_Customers.csv"))
(display "Enter number of clusters (k): ")
(define k (read))
;input handling
(while (not (real? k))
       (display "Please enter an integer value for k: ")
       (set! k (read))
       )
(define results (kmeans df k))
(display "Done clustering...\n")

(define clusters (car results))
(define final-centroids (cdr results))

(display "Size of each clusters:")
(for ([c clusters])
  (display "\n")
  (display (length c))
  )

(define (4-decimal-string n)
  (real->decimal-string n 4))

(display "\nFinal centroids:")
(for ([c final-centroids])
  (display "\n")
  (display (map 4-decimal-string c))
  )

(sdraw final-centroids #:null-style '/)








