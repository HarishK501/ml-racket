#lang racket

(require "dataframe-adt.rkt")

(define df (make-dataframe "penguins_preprocessed.csv"))
;(define df '())
;(define df null)
(null? (car df))
(col-selector df "i_Dream")