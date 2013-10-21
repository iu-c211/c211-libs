#lang racket

(define eni? exact-nonnegative-integer?)

(provide
 (contract-out
  [make-matrix        (->* (eni? eni?) (any/c) matrix?)]
  [matrix-generator   (-> eni? eni? (-> eni? eni? any/c) matrix?)]
   
  [matrix-cols        (-> matrix? eni?)]
  [matrix-rows        (-> matrix? eni?)]
  [matrix-ref         (-> matrix? eni? eni? any/c)]
  [matrix-set!        (-> matrix? eni? eni? any/c void)]
  [matrix-equal?      (-> matrix? matrix? boolean?)]
  
  [draw-matrix        (-> matrix? void)]
  [print-matrix       (-> matrix? void)]
  [print-matrix-cols  (parameter/c eni?)]
  [print-matrix-rows  (parameter/c eni?)]
  [print-matrix-width (parameter/c eni?)]))

(define-struct matrix (rows cols data)
  #:constructor-name new-matrix
  #:methods gen:custom-write
  [(define (write-proc image port mode)
     (fprintf port
              "#matrix<~a ~a>" 
              (matrix-rows image)
              (matrix-cols image)))])

(define (make-matrix rows cols [default 0])
  (new-matrix 
   rows cols 
   (for/vector ([r (in-range rows)])
     (for/vector ([c (in-range cols)])
       default))))

(define (matrix-generator rows cols f)
  (new-matrix 
   rows cols 
   (for/vector ([r (in-range rows)])
     (for/vector ([c (in-range cols)])
       (f r c)))))

(define (check-bounds function-name matrix row col)
  (unless (and (<= 0 row (- (matrix-rows matrix) 1))
               (<= 0 col (- (matrix-cols matrix) 1)))
    (error function-name "index (~a ~a) out of bounds for ~a" row col matrix)))

(define (matrix-ref matrix row col)
  (check-bounds 'matrix-ref matrix row col)
  (vector-ref (vector-ref (matrix-data matrix) row) col))

(define (matrix-set! matrix row col value)
  (check-bounds 'matrix-set! matrix row col)
  (vector-set! (vector-ref (matrix-data matrix) row) col value))

(define (matrix-equal? matrix1 matrix2)
  (equal? matrix1 matrix2))

(define (draw-matrix matrix) ; TODO
  (void))

(define (print-matrix matrix) ; TODO
  (void))

(define print-matrix-cols (make-parameter 5))

(define print-matrix-rows (make-parameter 5))

(define print-matrix-width (make-parameter 5))
