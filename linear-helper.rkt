#lang racket

;(require br/verbose-app)

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

(define (diff-sq x y)
 (square (- x y) )
 )

(define (sum-of-diff-sq lst m)
  
   (if (null? lst)
        0
   (+ (diff-sq (car lst) m) (sum-of-diff-sq (cdr lst) m))
  ))

(define(variance lst)
  
   (/ (sum-of-diff-sq lst (mean lst))(-(length lst) 1))
)
(define (covariance-inner lst1 m1 lst2 m2)
  (cond
    [(null? lst1) 0]
    [(null? lst2) 0]
    [else (+(*(-(car lst1) m1) (-(car lst2) m2))(covariance-inner (cdr lst1) m1 (cdr lst2) m2 ))]
  )
 )
(define (covariance l1 l2)
  (/ (covariance-inner l1 (mean l1) l2 (mean l2))(length l1)))

(define (coefficient_b1 l1 l2)
  (/(covariance l1 l2)(variance l1)))

(define (coefficient_b0 l1 l2)
  (-(mean l2)(*(coefficient_b1 l1 l2) (mean l1))))

(define (make_pred l1 l2 x)
   (+ (coefficient_b0 l1 l2)(* x (coefficient_b1 l1 l2)))
  )

(define (make_pred_list l1 l2 l3)

  (if (null? l3) '()
      
   (cons (make_pred l1 l2 (car l3)) (make_pred_list l1 l2 (cdr l3) ) )
      )
  )

(define (sub-list l i j)
  (if (or(null? l)(eq? i j)) '()
      (cons(list-ref l i)(sub-list l (+ i 1) j))
  ))

(define (rmse l1 l2)
  (if (or(null? l1)(null? l2)) 0
   (sqrt (/(+(square(- (car l1) (car l2))) (rmse (cdr l1) (cdr l2)))(length l1)))))

(define (mae l1 l2)
  (if (or(null? l1)(null? l2)) 0
    (/(+(abs(- (car l1) (car l2))) (mae (cdr l1) (cdr l2)))(length l1))))


(provide (all-defined-out))

