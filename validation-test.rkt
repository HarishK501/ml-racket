#lang racket
(require "dataframe-adt.rkt")

(define df (make-dataframe "penguins_preprocessed.csv"))
(define df2 '())
(define df3 null)

(displayln "Validation Tests \n")

(displayln "1. df")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if df
      #t
      #f)
  )

(displayln "2. df2")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if df2
      #t
      #f)
  )

(displayln "3. df3")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if df3
      #t
      #f)
  )

(displayln "4. (row-selector df 1)")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (row-selector df 1)
      #t
      #f)
  )

(displayln "5. (row-selector df -1)")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (row-selector df -1)
      #t
      #f)
  )

(displayln "6. (row-selector df 400)")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (row-selector df 400)
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

(displayln "11. (col-selector df 1)")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (col-selector df 1)
      #t
      #f)
  )

(displayln "12. (col-selector df -2)")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (col-selector df -2)
      #t
      #f)
  )

(displayln "13. (col-selector df \"i_Dream\")")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (col-selector df "i_Dream")
      #t
      #f)
  )

(displayln "14. (col-selector df \"abc\")")
(with-handlers ([exn:fail? (lambda (v)
                             ((error-display-handler) (exn-message v) v))])
  (if (col-selector df "abc")
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
