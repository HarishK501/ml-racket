#lang racket

(require csv-reading)

(define make-galapagos-csv-reader
  (make-csv-reader-maker
   '((separator-chars              #\,)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

(define (all-rows path make-reader)
  (define next-row (make-reader (open-input-file path)))
  (define (loop)
    (define row (next-row))
    (if (empty? row)
        '()
        (cons row (loop))))
  (loop))

(define dataset (all-rows "penguins_preprocessed.csv" make-galapagos-csv-reader))

(define (row-selector df n)
  (list-ref df n))

(define (col-selector df n)
  (if (null? df)
      '()
      (cons (list-ref (car df) n) (col-selector (cdr df) n))
      ))


#|
 - unique values in a column
 - null values
 - 

|#




