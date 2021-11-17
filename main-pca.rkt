#lang racket

(require math/statistics)
(require "dataframe-adt.rkt")
(require "pca.rkt")
(require "pca-helper.rkt")

(define data (make-dataframe "iris_data.csv"))
(define labels (make-dataframe "iris_labels.csv"))
(principal-component-analysis data labels petal-width)
