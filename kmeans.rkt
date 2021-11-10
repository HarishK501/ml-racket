#lang racket

(require "distance-metrics.rkt")
(require "dataframe-adt.rkt")

(define (kmeans data k)
  (define L (no-of-records data))
  L)

(define df (make-dataframe "penguins_data.csv"))
(kmeans df 3)