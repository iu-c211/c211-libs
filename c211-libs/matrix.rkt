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
  
  [matrix->vov        (-> matrix? (vectorof vector?))]
  [vov->matrix        (-> (vectorof vector?) matrix?)]
    
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

(define (vov->matrix vov)  
  (let ((lens (map vector-length (vector->list vov)))
        (lenlen (length lens)))
    (cond
      [(null? lens) (new-matrix 0 0 #())]
      [(or (= lenlen 1) (apply = lens)) (new-matrix lenlen (car lens) vov)]
      [else (error 'vov->matrix "Rows were not the same length.  Row lengths:\n~a\n" lens)])))

(define matrix->vov matrix-data)

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
  (error 'draw-matrix "FinishMe - this function is not complete yet.  It is not needed for current assignments.")
  (void))

(define (print-matrix matrix) ; TODO
  (let loop ((i 0) (j 0))
    (cond
      [(= i (matrix-rows matrix)) (void)]
      [(= j (matrix-cols matrix)) (newline) (loop (add1 i) 0)]
      [else (printf "~a " (matrix-ref matrix i j))
            (loop i (add1 j))])))

(define print-matrix-cols (make-parameter 5))

(define print-matrix-rows (make-parameter 5))

(define print-matrix-width (make-parameter 5))
