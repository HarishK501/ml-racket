#lang racket
(require "naive-bayesian-helper.rkt")
;Bayesian takes two vectors - independent and response
;Calculate Conditional and Class Probabilities

;The Bayesian Model Generation. Returns a procedure to predict
;The class and conditional probabilites are used here as states
(define (naive-bayesian-fit Xtrain ytrain (verbose false) (get_cp false))
 
  (check-categorical Xtrain)
  (check-categorical ytrain)

  ;State Values
  (define class-prob (get-class-prob ytrain))
  (define cond-prob-tab (list))

  (for ((i (in-range 0 (length (car Xtrain)))))
    (set! cond-prob-tab (append cond-prob-tab (list (list i (get-cond-prob (get-column Xtrain i) ytrain))))))

  (define uni-lab (remove-duplicates ytrain))

  (define (select-cp attr class val)
  
  ;Get Attribute Wise Conditional Probability
  (define (get-attr-cp idx)
    (if (and (> (length cond-prob-tab) idx) (> idx -1))
        (cadr (list-ref cond-prob-tab idx))
        (error "get-attr-cp index out of bounds")))

  ;Get Class Probability from  Attribute conditional Probability
  (define (get-cl-prob class cptab)
    (cond ((null? cptab) (error "get-cl-prob class not found" class))
           ((eq? (car class) (caaar cptab)) (cadar cptab))
           (else (get-cl-prob class (cdr cptab))))
    )
    

    (define (sel-helper cpl)
      
      (cond ((null? cpl) (error "sel-helper value not found in column" val))
             ((equal? (caar cpl) val) (cadar cpl))
             (else (sel-helper (cdr cpl)))))


   (sel-helper (get-cl-prob class (get-attr-cp attr)))
  )


 (define (loc-class-prob class cp)
   (cond ((null? cp) (error "loc-class-prob class not found" class))
         ((eq? (car class) (caaar cp)) (caddar cp))
         (else (loc-class-prob class (cdr cp))))) 

  (if get_cp (displayln cond-prob-tab) #f)
  
  (if verbose
      (begin
        (print-cond-prob cond-prob-tab)
        (display (length ytrain))
        (displayln " rows modelled")
        (displayln "")
        )
      (displayln "Model Generated!"))
  ;Defining lambda to be returned. Takes Xtest and returns predictions
  (lambda (Xtest)
    ;Check if all are discrete values
    (check-categorical Xtest)

    ;Final Prediction that is to be returned
    (define pred (list))

    (define (make-pred row)

      (define class "")
      (let ((lenr (length row))
            (max-prob -1)
             (prob 0))
      
      (for ((i uni-lab))
        (define prob-list (list ))
        
        (for ((j (in-range 0 lenr)))
          (set! prob-list (append prob-list (list (select-cp j i (list-ref row j))))))

        (set! prob (accumulate * (loc-class-prob i class-prob) identity prob-list))
        ;setting the class to be assigned
        (if (> prob max-prob)
            (begin (set! max-prob prob)
                   (set! class i))
            #t)
        )
      )
      (set! pred (append pred class)))

    
    (map make-pred Xtest)

    (display (length Xtest)) (displayln " rows predicted.")
    pred
    )
  

  )


(define (accuracy pred truth)
  (define (match pred truth res)
    (if (null? pred)
        res
        (if (equal? (car pred) (caar truth))
            (match (cdr pred) (cdr truth) (+ res 1))
            (match (cdr pred) (cdr truth) res)
            )
        )
  )
  (define res (match pred truth 0))
  (* (/ res (length pred)) 100.0)
)

(provide (all-defined-out))
