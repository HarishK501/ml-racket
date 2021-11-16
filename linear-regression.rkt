#lang racket
(require "dataframe-adt.rkt")
(require "list-helper-functions.rkt")
(require "linear-helper.rkt")
(require "knn.rkt")

(define data (make-dataframe "linear_data.csv"))
(define labels (make-dataframe "linear_data.csv"))

(define lst1 (col-selector data 0))
(define lst2 (col-selector data 1))

(define l1 (sub-list lst1 1 (*(quotient(length lst1)8)7) ))
(define l2 (sub-list lst2 1 (*(quotient(length lst2)8)7) ))
(define l3 (sub-list lst1 (*(quotient(length lst1)8)7) (length lst1) ))
(define l4 (sub-list lst2 (*(quotient(length lst2)8)7) (length lst2) ))

(make_pred_list l1 l2 l3)

l4

(rmse (make_pred_list l1 l2 l3) l4)


(mae (make_pred_list l1 l2 l3) l4)



