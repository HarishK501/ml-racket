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


#|
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
|#

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

(provide (all-defined-out))

