#lang racket
(require "dataframe-adt.rkt")
(require "list-helper-functions.rkt")
(require "linear-helper.rkt")
(require "knn.rkt")

(define data (make-dataframe "Salary_Data.csv"))
(define labels (make-dataframe "Salary_Data.csv"))

(define lst1 (col-selector data 0))
(define lst2 (col-selector data 1))

(define l1 (sub-list lst1 1 (*(/(no-of-records data)5)4) ))
(define l2 (sub-list lst2 1 (*(/(no-of-records data)5)4) ))
(define l3 (sub-list lst1 (*(/(no-of-records data)5)4) (no-of-records data) ))
(define l4 (sub-list lst2 (*(/(no-of-records data)5)4) (no-of-records data) ))

(make_pred_list l1 l2 l3)

l4

(rmse (make_pred_list l1 l2 l3) l4)






