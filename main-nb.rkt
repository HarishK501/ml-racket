#lang racket

(require "dataframe-adt.rkt")
(require "naive-bayesian.rkt")
(require "naive-bayesian-helper.rkt")

(define data (make-dataframe "data-nb.csv"))
(define labels (make-dataframe "labels-nb.csv"))

(define train-x (row-selector data 1 1000))
(define train-y (row-selector labels 1 1000))
(define test-x (row-selector data 1001 1200))
(define test-y (row-selector labels 1001 1200))

(define model (naive-bayesian-fit train-x train-y false true))

(define predicted (model test-x) )


(display " Accuracy: ")
(display (accuracy predicted test-y))