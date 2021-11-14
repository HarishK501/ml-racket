#lang racket
(require "dataframe-adt.rkt")
(require "list-helper-functions.rkt")
(require "linear.rkt")

(define data (make-dataframe "Salary_Data.csv"))
(define labels (make-dataframe "Salary_Data.csv"))

(define l1 (col-selector data 0))
(define l2 (col-selector data 1))




