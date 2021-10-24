#lang racket

(require "dataframe-adt.rkt")

(define data (make-dataframe "penguins_data.csv"))
data
(define labels (make-dataframe "penguins_labels.csv"))
labels

;(define df '())
;(define df null)
;(null? (car df))
;(col-selector df "i_Dream")
;(row-selector data 1 4)