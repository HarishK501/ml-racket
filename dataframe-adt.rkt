#lang racket

(require csv-reading)

(define (make-dataframe path)
  
  (define make-df-csv-reader
    (make-csv-reader-maker
     '((separator-chars              #\,)
       (strip-leading-whitespace?  . #t)
       (strip-trailing-whitespace? . #t))))

  (define m 0)
  (define n -1)
  
  (define (all-rows path make-reader)
    (define next-row (make-reader (open-input-file path)))
    (define row (next-row))

    (if (null? row)
        (set! m 0)
        (set! m (- (length row) 1))
        )
    
    (define (loop)
      (set! n (+ n 1))
      (if (> n 0)
          (set! row (next-row))
          (set! n 0)
          )
             
      (if (empty? row)
          ((lambda (x)
             (if (> n 0)
                 (set! n (- n 1))
                 (set! n 0)
                 )
             '())
           1)
          (cons row (loop))))
    (loop)
    )

  (list (all-rows path make-df-csv-reader) n m)
  
  )

(define (index-within-bounds? df i tag)
  (define upper-limit 0)
  (cond
    ;row index
    ((eq? tag 'r)
     (set! upper-limit (cadr df))
     #t
     )
    ;column index
    ((eq? tag 'c)
     (set! upper-limit (caddr df))
     #t
     )
    )
  
  (if (and (> i -1) (< i upper-limit))
      #t
      ((lambda (i)
         (raise-argument-error 'index-within-bounds? (string-append "[0, " (number->string upper-limit) "].") i)
         #f)
       i))
  )

(define (feature-index df f)
  (define i (index-of (caar df) f))
  (if i
      i
      ((lambda (f)
         (error "Feature does not exist:" f)
         #f)
       f)
      )
  )

(define (row-selector df i [j 0])
  (cond
    ((and (not (null? df)) (not (null? (car df))) (index-within-bounds? df i 'r))
     (list-ref (car df) i))

    (else
     (error "Dataframe must not be null.")
     #f)
    )
  )

#| (define (string-element-of-list lst str)
  (findf (lambda (arg) (string=? arg str))
         lst)
  )
|#

(define (col-selector df x [y 0])

  (define (col-selector-helper df i)
    (cond
      ((null? (cdr df))
       (cons (list-ref (car df) i) '()))
    
      (else
       (cons (list-ref (car df) i) (col-selector-helper (cdr df) i))
       )
      )
    )

  (define col '())
  
  (cond
    ;Select by feature name
    ((and (not (null? df)) (not (null? (car df))) (string? x))
     (col-selector df (feature-index df x)))

    ;Select by index

    ((cond
      ((or (null? df) (null? (car df)))
       (error "Dataframe must not be null."))

      ((not (index-within-bounds? df x 'c)))

      (else
       (set! col (col-selector-helper (car df) x)))
     )
     col)
      
    (else
     (error "Dataframe must not be null.")
     #f)
    )
  )

#|
 - Slicing
 - unique values in a column
 - null values
 - 

|#

(provide (all-defined-out))
