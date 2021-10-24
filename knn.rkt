#lang racket

(require "dataframe-adt.rkt")

(define (euclidean-distance p1 p2)
  (sqrt (for/sum
            ([x1 p1] [x2 p2])
          (expt (- x1 x2) 2))))

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

(define (knn-classifier k train test) '())




