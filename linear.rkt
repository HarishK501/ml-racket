#lang racket

(define (sum-series lst proc)
    (if (null? lst)
        0
        (+ (car (map proc lst)) (sum (cdr (map proc lst))))))

(define (sum lst)
  (sum-series lst (lambda(x)x)))

(define (mean lst)
    (/ (sum lst) (length lst)))

(define (square x)
  (* x x))

(define (sum-of-squares lst)
  (sum-series lst square))

(define (sq-diff x y)
 (- (square x) (square y))
 )

(define(variance lst)
  (
   (if (null? lst)
        0
        (+ (sq-diff (car lst)(mean lst)) (variance (sq-diff (cdr lst)(mean lst))))
    )
   )
  )
  
