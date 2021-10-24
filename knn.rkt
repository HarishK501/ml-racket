#lang racket

(require "dataframe-adt.rkt")

(define (euclidean-distance p1 p2)
  (sqrt (for/sum
            ([x1 p1] [x2 p2])
          (expt (- x1 x2) 2))))

#|(define (manhattan-distance p1 p2)
  (sqrt (for/sum
            ([x1 p1] [x2 p2])
          (expt (- x1 x2) 2))))|#

; to find frequency of an element in a list
(define (freq elem lst)
  (let f ((_lst lst) (c 0))
    (if (null? _lst)
        c
        (if (eq? (car _lst) elem)
            (f (cdr _lst) (+ c 1))
            (f (cdr _lst) c)))))

; returns the first value that has the highest frequency
(define (mode lst)
  (let loop ((_lst lst)         
             (max-counter 0)    
             (max-current #f)) 
    (if (null? _lst)            
        max-current             
        (let ((n (freq (car _lst) lst))) 
          (if (> n max-counter) 
              (loop (cdr _lst) n (car _lst))
              (loop (cdr _lst) max-counter max-current))))))



(define (min lst)
  (define (min-helper lst acc)
  (cond
    ((null? lst) acc)
    ((< (car lst) acc)
     (min-helper (cdr lst) (car lst)))
    (else
     (min-helper (cdr lst) acc))))
  (if (null? lst)
      #f
      (min-helper (cdr lst) (car lst))))

(define (knn-classifier k train-x train-y test)
  (define dp '())
  (define labels '())
  (for ((i (in-range 1 (no-of-records test))))
        (set! dp (row-selector test i))
        (define distances (list ))
    
        ;loop to run through elements of train
        (for ((j (in-range 0 (no-of-records train-x))))
           (set! distances (append distances (euclidean-distance dp (row-selector train-x j))))
           )

        (define neighbors (list ))
        ;loop to find first k neighbors
        (for ((j (in-range 0 k)))
          (define mindist (min distances))
         (set! neighbors (append neighbors (get-class train-y (+ (index-of distances mindist) j))))
         (set! distances (remove mindist distances)) 
          )
        ;find mode and assign to test object
        (set! labels (append labels (mode neighbors)))
    )
  labels)


(provide (all-defined-out))

