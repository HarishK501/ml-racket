#lang racket

(require math/statistics)
(require "dataframe-adt.rkt")

(provide (all-defined-out))

(define identity
    (lambda (x)
      x))

;;used for data abstractions

;; data helper functions

(define class
  (lambda (x)
    (if (eqv? x 'name)
        "Class"
        (car (cdr (cdr (cdr (cdr x))))))))


(define (same-class class1 class2)
  (string=? class1 class2))

;; function to remove last element from list, needed to remove class from iris csv to form an array
;; and to remove the last element
(define (remove-last lst)
  (reverse (cdr (reverse lst))))

;; similar to csv only just drops the column, no newline list at the end of a csv
(define (remove-last-col lst-of-lsts)
  (map (lambda (x) (remove-last x)) lst-of-lsts))

;; helper function to append the class column back onto a list after computing the clustering
;; takes a list and and element and appends it to the end
(define (append-last lst elm)
  (reverse (append (list elm) (reverse lst))))


;; Data manipulation functions  

;; calculate mean and standard deviation for data
(define (col-means col-sums l)
  (map (lambda(x) (/ x l)) col-sums)
)

;; standardize the matrix by calculating z score for each data point
; function that takes a NXM data and standardizes the values (z = (x - mean) / std)
(define (standardize-matrix df iris-mean iris-std)

  (define (standardize-column col mean std)

    (define z-score
      (lambda(x)
        (/ (- x mean) std) 
      )
    )

    (map z-score col)
  )

  (define (enumerate-column-standardize df i j)
    (if (> i j)
        '()
        (cons (standardize-column (cdr (col-selector df i)) (list-ref iris-mean i) (list-ref iris-std i)) (enumerate-column-standardize df (+ 1 i) j)))
  )

  (enumerate-column-standardize df 0 (- (no-of-features df) 1))
)

;;Data abstractions of iris dataset

(define petal-width
  (lambda (x)
    (if (eqv? x 'name)
    "Petal Width"
    (car x))))

(define sepal-length
  (lambda (x)
    (if (eqv? x 'name)
        "Sepal Length"
        (car (cdr x)))))

(define sepal-width
  (lambda (x)
    (if (eqv? x 'name)
        "Sepal Width"
        (car (cdr (cdr x))))))

(define petal-length
  (lambda (x)
    (if (eqv? x 'name)
         "Petal Length"
         (car (cdr (cdr (cdr x)))))))