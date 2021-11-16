#lang racket
;All Helper Functions are implemented here

;frequency of an element in a list
(define (freq elem lst)
  (let f ((_lst lst) (c 0))
    (if (null? _lst)
        c
        (if (equal? (car _lst) elem)
            (f (cdr _lst) (+ c 1))
            (f (cdr _lst) c)))))

(define (lab-freq elem lst)
  (let f ((_lst lst) (c 0))
    (if (null? _lst)
        c
        (if (eq? (caar _lst) (car elem))
            (f (cdr _lst) (+ c 1))
            (f (cdr _lst) c)))))

;Accumulate                                       HIGHER ORDER FUNCTION
(define (accumulate op acc proc lst)
  (cond ((null? lst) acc)
        (else (accumulate op (* acc (proc (car lst))) proc (cdr lst)))))

;get-column returns list of all rows under a column by index
(define (get-column df idx)
  (define column (list ))

  (define (get-column-helper rows)
    (if (null? rows)
        true
        (begin
          (set! column (append column (list (list-ref (car rows) idx))))
          (get-column-helper (cdr rows)))
        ))
  (get-column-helper df)
  column)


;check categorical to check if all columns contain discrete values      FILTER
(define (check-categorical df)

  (define (check-helper row)
    (if (= (length row) (length (append (filter string? row) (filter exact-integer? row))))
        #t
        (error "All attributes should be discrete!"))
    )

  (for ((i (in-range 0 (length df))))
    (check-helper (list-ref df i))))

;Store class probabilities in a list
(define (get-class-prob labels)
  (define class-prob (list ))

  (let ((leny (length labels))
        (unique (remove-duplicates labels))
        (lenuniq (length (remove-duplicates labels))))
    
    (for ((i (in-range 0 lenuniq)))
      (define class (list-ref unique i))
      (define value (list (lab-freq class labels) (/ (lab-freq class labels) leny)))

      (set! class-prob (append class-prob (list (cons class value))))))
  class-prob
  )

;Store conditional probabilities
(define (get-cond-prob attr labels)
  (define uni-lab (remove-duplicates labels))
  
  (define cond-prob (list ))
  (define un-attr (remove-duplicates attr))
  (define no-un (length un-attr))

  (for ((i (in-range 0 (length uni-lab))))
    (define class (list-ref uni-lab i))

    (define ids (indexes-of labels class))

    (define rows (list))

    (for ((j (in-range 0 (length ids))))

      (set! rows (append rows (list (list-ref attr (list-ref ids j))))))

    ;(displayln rows)
    (define len-rows (length rows))

    (define one-c-prob (list))
    (for ((j (in-range 0 no-un)))
      (define att-val (list-ref un-attr j))
      ;(displayln att-val)
      ;(displayln (freq att-val rows))
      (set! one-c-prob (append one-c-prob (list (list att-val (/ (freq att-val rows) len-rows))))))

    (set! cond-prob (append cond-prob (list (list class one-c-prob)))))

  cond-prob
  )

(define (print-cond-prob cpt)
  (for ((i cpt))
    (display "Attribute: ") (displayln (car i))

    (for ((j (cadr i)))
      (display "Label: ") (display (caar j))

      (display "   (Value/Probability: ")
      (for ((k (cadr j)))
        (display k) (display "   "))

      (displayln ""))
    (displayln "")
    )
    )


(provide (all-defined-out))