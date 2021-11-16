#lang racket

(require csv-reading)
(provide (all-defined-out))

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
    (define tag (car row))
    (define parsed null)

    (if (null? row)
        (set! m 0)
        (set! m (- (length row) 1))
        )
    
    (define (loop)
      (set! n (+ n 1))
      (if (> n 0)
          ((lambda(x)
            (set! row (next-row))
             (set! parsed (filter string->number row))
          
             (if (null? parsed)
                 #t
                 (set! row (map string->number parsed))
                 ))
            1)
     
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
  
  (if (and (> i -1) (<= i upper-limit))
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

(define (row-selector df i [j -1])

  (define (row-selector-helper i)
    (if (index-within-bounds? df i 'r)
        (list-ref (car df) i)
        #f)
    )

  (define (row-slicer-helper df i j)
    (if (> i j)
        '()
        (cons (row-selector-helper i) (row-slicer-helper df (+ 1 i) j)))
    )
  
  (define row null)
  (cond

    ((or (null? df) (null? (car df)))
     (error "Dataframe must not be null."))
        
    ((= j -1) (row-selector-helper i))

    ((not (index-within-bounds? df j 'r))
     #f)
        
    ((>= j i)
     (row-slicer-helper df i j)
     )

    (else
     (raise-argument-error 'row-selector (string-append "[" (number->string i) ", " (number->string (cadr df)) "]") j))
            
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

(define (no-of-records data)
  (cadr data)
  )

(define (no-of-features data)
  (+ (caddr data) 1)
  )

(define (get-class labels i)
  (car (list-ref (car labels) i))
  )


(define (train-test-split data labels frac)

  
  (define (frac-to-num)
    (if (or (not (< frac 1)) (not (> frac 0)))
        (error "The fraction must be in the range (0,1)")
        (floor (* frac (no-of-records data)))))
  
  ;Gets list of rows, returns rows, number of rows and number of columns
  (define (data-set-maker list-of-rows)
    (list list-of-rows (length list-of-rows) (length (car list-of-rows))))
  
  ;list containing train-x, test-x, train-y, test-y
  (define split-datasets (list ))

  (set! split-datasets (append split-datasets (data-set-maker (row-selector data 1 (frac-to-num frac)))))
  (set! split-datasets (append split-datasets (data-set-maker (row-selector data (+ (frac-to-num frac) 1) (+ 1 (no-of-records data))))))
  (set! split-datasets (append split-datasets (data-set-maker (row-selector labels 1 (frac-to-num frac)))))
  (set! split-datasets (append split-datasets (data-set-maker (row-selector labels (+ (frac-to-num frac) 1) (+ 1 (no-of-records data))))))
  
  split-datasets)

(define (all-column-operation df proc1 proc2)

  (define (column-operation col proc1 proc2)
    (if (null? proc1)
      (proc2 col)        ; e.g. (stddev col)
      (proc1 proc2 col)  ; e.g. (apply + col)
    )
  )

  (define (enumerate-column-accumulate-result df i j proc1 proc2) 
    (if (> i j)
        '()
        (cons (column-operation (cdr (col-selector df i)) proc1 proc2) (enumerate-column-accumulate-result df (+ 1 i) j proc1 proc2)))
  )

  (enumerate-column-accumulate-result df 0 (- (no-of-features df) 1) proc1 proc2)
)

(define (transpose matrix)                   
  (for/list ((i (length (list-ref matrix 0)))) ; sends result in form of a list
    (for/list ((il matrix)) ; access the rows of the matrix                   
      (list-ref il i)))) ; access all the rows of ith column (il row ith col)
