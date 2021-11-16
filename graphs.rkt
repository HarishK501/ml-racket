#lang racket

(require plot)
(require "pca-helper.rkt")
(provide plot-3D)

(define (plot-3D list-of-datasets col1 col2 col3)
  (define (3D-points-creator list-of-datasets col1 col2 col3 list-of-points count)
    (if (null? list-of-datasets)
         list-of-points
         (3D-points-creator (cdr list-of-datasets) col1 col2 col3
                            (cons (points3d
                                   (foldr (lambda(x y)
                                            (cons
                                             (list
                                              (string->number (col1 x))
                                              (string->number (col2 x))
                                              (string->number (col3 x))) y))
                                          '() (car list-of-datasets))
                                   #:label (string-append "Dataset " (number->string count))
                                   #:color count) list-of-points) (+ count 1))))
     (plot3d (3D-points-creator list-of-datasets col1 col2 col3 '() 1)
             #:title (string-append (col1 'name) " vs " (col2 'name) " vs " (col3 'name))
             #:x-label (col1 'name)
             #:y-label (col2 'name)
             #:z-label (col3 'name)))
