#lang racket

(define (euclidean-distance p1 p2)
  (sqrt (for/sum
            ([x1 p1] [x2 p2])
          (sqr (- x1 x2)))))

(define (manhattan-distance p1 p2)
  (for/sum
      ([x1 p1] [x2 p2])
    (abs (- x1 x2))))

(provide (all-defined-out))