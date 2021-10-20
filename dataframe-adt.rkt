#lang racket

(require csv-reading)

(define (make-dataframe path)
  
  (define make-df-csv-reader
    (make-csv-reader-maker
     '((separator-chars              #\,)
       (strip-leading-whitespace?  . #t)
       (strip-trailing-whitespace? . #t))))

  (define m 0)
  (define n 0)
  
  (define (all-rows path make-reader)
    (define next-row (make-reader (open-input-file path)))
    (define row (next-row))
    (set! m (- (length row) 1))
    (if (> m -1)
        ((lambda (n)
           (set! n 1)
           (define (loop)
             (define row (next-row))
             (set! n (+ n 1))
             (if (empty? row)
                 '()
                 (cons row (loop))))
           (loop))
         n)
         (set! n 0))
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
      (define i (index-of (row-selector df 0) f))
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
        ((and (not (null? df)) (index-within-bounds? df i 'r))
         (list-ref df i))

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
      (cond
        ;Select by feature name
        ((and (not (null? df)) (string? x))
         (col-selector df (feature-index df x)))

        ;Select by index
        ((and (not (null? df)) (null? (cdr df)))
         (cons (list-ref (car df) x) '()))
    
        ((and (not (null? df)) (index-within-bounds? df x 'c))
         (cons (list-ref (car df) x) (col-selector (cdr df) x))
         )
    
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
    