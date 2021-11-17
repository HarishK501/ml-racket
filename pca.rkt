#lang racket

(require net/url)
(require csv-reading)
(require math/array)
(require math/matrix)
(require math/statistics)
(require plot)

;; require custom functions 
(require "graphs.rkt")
(require "pca-helper.rkt")
(require "dataframe-adt.rkt")

(provide principal-component-analysis)

(define (principal-component-analysis data labels plot-param)
;; define an array with the sum of each column 
(define data-sum (all-column-operation data apply +))

;; function that takes one argument, a dataframe and returns the mean of every column
(define data-mean (col-means data-sum (no-of-records data) ))

;; function that takes one argument, a dataframe and returns the std of every column
(define data-std (all-column-operation data null stddev))

;; define standardized data
(define z (transpose (standardize-matrix data data-mean data-std)))
(set! z (cons z (cons (- (length z)) (cons (- (length (car z)) 1) '()))))

;; calculate mean vector (mean of z)
(define z-sum (all-column-operation z apply +))
(display "\n\nStandardized Dataset\n")
(display z-sum)

(define mean-vector (col-means z-sum (+ (no-of-records z) 1)))

(define z1 (list*->array (car z) number?))
(define mean-vector1 (list->array mean-vector))
(display "\n\nMean vector of Standardized Dataset\n")
(display mean-vector)

;; create N X N matrix of containing covariance of all properties,
;; cov[x, y] = ∑i (xi – E[x]) (yi – E[y])  / (n-1)
;; (array z - mean vector transposed) multiplied by (z - mean vector) all divided by
;; N - 1 (149 in this case)

(define data-co-variance-matrix
 (array/
 (matrix* (array-axis-swap (array- z1 mean-vector1) 1 0) (array- z1 mean-vector1))
 (array (- (vector-ref (array-shape z1) 0) 1))))
(display "\n\nCo-variance Matrix\n")
(display data-co-variance-matrix)
           
;; eigenvectors and eigenvalues from the covariance matrix
(define data-eigenvalues
  (array #[2.93035378  0.92740362  0.14834223  0.02074601]))
           
(define data-eigenvectors
  (array #[#[0.52237162 -0.37231836 -0.72101681  0.26199559]
           #[-0.26335492 -0.92555649  0.24203288 -0.12413481]
           #[0.58125401 -0.02109478  0.14089226 -0.80115427]
           #[0.56561105 -0.06541577  0.6338014   0.52354627]]))

;; use eigenvectors with the 2 or 3 highest eigenvalues to create projection matrix
;; this function takes two arguments, the first is an array of eigenvectors, the second is the number
;; number of dimensions for the new array

;; take the first 3 columns (taking 3 Principle Components) from the corresponding eigenvector/value pairs 
(define data-projection-matrix
  (list*->array (map (lambda (x) (remove-last x)) (array->list* data-eigenvectors)) number?))

;; plot with result of dot product
;; z multiplied by the projection-matrix
;; Final DataSet = FeatureVector * StandardizedOriqinalDataSet
(define data-pca
  (array->list* (matrix* z1 data-projection-matrix)))

(define data-pca-classes
  (map (lambda (x y) (append-last x y)) data-pca
       (cdar (transpose (car labels)))))

(display "\n\nFinal Dataset\n")
(display data-pca-classes)

(define pca-class
  (lambda (x)
    (if (eqv? x 'name)
         "Class"
         (car (cdr (cdr (cdr x)))))))

(define filter-pca-cols 
  (lambda (data parm expr [class-t "none"])
    (if (same-class class-t "none")
        (foldr (lambda (x y) (if (expr (parm x)) (cons x y) y)) '() data)
        ;;filter then get the average
        (foldr (lambda (x y) (if (expr (parm x)) (cons x y) y)) '()
               (foldr (lambda (x y) (if (same-class (pca-class x) class-t) (cons x y) y))
                      '() data)))))

(let ((col plot-param))
(define pca1 (remove-last-col (filter-pca-cols data-pca-classes col identity "Iris-setosa")))
(define pca2 (remove-last-col (filter-pca-cols data-pca-classes col identity "Iris-versicolor")))
(define pca3 (remove-last-col (filter-pca-cols data-pca-classes col identity "Iris-virginica")))

(display "\n\nPlotting the Principle Components\n")
;; 3d plot of pca of dataset
(plot3d (list (points3d pca1 #:sym 'dot #:size 20 #:color 1 #:label "Iris-setosa")
              (points3d pca2 #:sym 'dot #:size 20 #:color 2 #:label "Iris-versicolor")
              (points3d pca3 #:sym 'dot #:size 20 #:color 3 #:label "Iris-virginica"))
        #:altitude 25
        #:title "3D PCA of dataset")
)
)

