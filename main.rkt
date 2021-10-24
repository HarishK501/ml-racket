#lang racket

(require "dataframe-adt.rkt")
(require "knn.rkt")

(define data (make-dataframe "penguins_data.csv"))
;data
(define labels (make-dataframe "penguins_labels.csv"))
;labels
;(get-class labels 2)
;(row-selector data 0)

;(define df '())
;(define df null)
;(null? (car df))
;(col-selector df "i_Dream")
;(row-selector data 1 4)
(define train-x (row-selector data 1 240))
(define train-y (row-selector labels 1 240))
(define test-x (row-selector data 241 333))
(define test-y (row-selector labels 241 333))
;(knn-classifier 3 train-x train-y test-x)
(no-of-records test)