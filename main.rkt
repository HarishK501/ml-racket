#lang racket

(require "dataframe-adt.rkt")
(require "knn.rkt")

(define data (make-dataframe "penguins_data.csv"))
(define labels (make-dataframe "penguins_labels.csv"))
;labels
;(get-class labels 2)
;(row-selector data 0)

;(define df '())
;(define df null)
;(null? (car df))
;(col-selector df "i_Dream")
;(row-selector data 1 4)
(define train-x (row-selector data 1 250))
(define train-y (row-selector labels 1 250))
(define test-x (row-selector data 251 333))
(define test-y (row-selector labels 251 333))

(define predicted (knn-classifier 3 train-x train-y test-x))

predicted

(accuracy predicted test-y)

;(no-of-records test-x)