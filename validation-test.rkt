#lang racket
(require "dataframe-adt.rkt")
(require "knn.rkt")

(define data (make-dataframe "penguins_data.csv"))
(define labels (make-dataframe "penguins_labels.csv"))
(define df2 '(() 0 0))
(define df3 null)

(displayln "Validation Tests \n")

(displayln "1. data")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if data
      #t
      #f)
  )

(displayln "2. (null? df2)")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (null? df2)
      #t
      #f)
  )

(displayln "3. (null? df3)")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (null? df3)
      #t
      #f)
  )

(displayln "4. (row-selector data 1)")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (row-selector data 1)
      #t
      #f)
  )

(displayln "5. (row-selector data -1)")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (row-selector data -1)
      #t
      #f)
  )

(displayln "6. (row-selector data 400)")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (row-selector data 400)
      #t
      #f)
  )

(displayln "7. (row-selector df2 -1)")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (row-selector df2 -1)
      #t
      #f)
  )

(displayln "8. (row-selector df2 1)")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (row-selector df2 1)
      #t
      #f)
  )

(displayln "9. (row-selector df3 -1)")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (row-selector df3 -1)
      #t
      #f)
  )

(displayln "10. (row-selector df3 1)")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (row-selector df3 1)
      #t
      #f)
  )

(displayln "11. (col-selector data 1)")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (col-selector data 1)
      #t
      #f)
  )

(displayln "12. (col-selector data -2)")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (col-selector data -2)
      #t
      #f)
  )

(displayln "13. (col-selector data \"i_Dream\")")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (col-selector data "i_Dream")
      #t
      #f)
  )

(displayln "14. (col-selector data \"abc\")")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (col-selector data "abc")
      #t
      #f)
  )

(displayln "15. (col-selector df2 \"i_Dream\")")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (col-selector df2 "i_Dream")
      #t
      #f)
  )

(displayln "16. (col-selector df3 \"i_Dream\")")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (col-selector df3 "i_Dream")
      #t
      #f)
  )

(displayln "17.#f (row-selector data 4 1)")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (row-selector data 4 1)
      #t
      #f)
  )

(displayln "18.#f (row-selector data 4 -2)")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (row-selector data 4 -2)
      #t
      #f)
  )

(displayln "19.#t (row-selector data 4 -1)")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (row-selector data 4 -1)
      #t
      #f)
  )

(displayln "20.#t (row-selector data 4 4)")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (row-selector data 4 4)
      #t
      #f)
  )

(displayln "21.#t (row-selector data 4 6)")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (row-selector data 4 6)
      #t
      #f)
  )

(define train-x (row-selector data 1 250))
(define train-y (row-selector labels 1 250))
(define test-x (row-selector data 251 333))
(define test-y (row-selector labels 251 333))
(define predicted (knn-classifier 3 train-x train-y test-x))

(displayln "22.#t (define predicted (knn-classifier 3 train-x train-y test-x))")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if predicted
      predicted
      #f)
  )

(define acc (accuracy predicted test-y))

(displayln "23.#t (accuracy predicted test-y)")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if acc
      acc
      #f)
  )