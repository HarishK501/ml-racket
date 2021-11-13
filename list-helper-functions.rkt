#lang racket
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


; returns minimum of list
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

; returns centroid of points
(define (centroid points)
  (define L (length points))
  (map (lambda (ls) (* 1.0 (/ (apply + ls) L)))
       (apply map list points)))

(provide (all-defined-out))




