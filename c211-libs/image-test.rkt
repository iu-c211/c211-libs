#lang racket
(require rackunit
         "image.rkt")

(define (cout . x)
  (if (null? x)
      x
      (and (display "\n")(display (car x))))
      (display "\n"))

(cout "Beginning tests...")
(define i (make-image 50 25 (color 0 0 0)))
(define ibl (make-image 200 100))
(define iclr (make-image 64 256 (lambda (r c) (color r c (quotient (+ r c) 2)))))
(define i256 (make-image 256 256 (lambda (r c) (color (random 256) (random 256) (random 256)))))

(cout "Simple image creation")
(time (begin
(make-image 50 25 (color 0 0 0))
(make-image 200 100)
(make-image 64 256 (lambda (r c) (color r c (quotient (+ r c) 2))))
(make-image 256 256)
(void)
))

(cout "Big image creation")
(time (begin
(make-image 500 250 (color 0 0 50))
(make-image 2000 1000)
(make-image 640 2560 (lambda (r c) (color (min r 255) (min c 255) (min (max (- r c) (- c r)) 255))))
(make-image 2560 1600 (color 255 255 255))
(void)
))


(cout "Simple width and height")
(time (begin
(check = 50 (image-rows i) "make-image: Image width")
(check = 25 (image-cols i) "make-image: Image height")
(check = 50 (image-rows i) "make-image: Image width")
(check = 25 (image-cols i) "make-image: Image height")
))

(cout "Color equality")
(time
 (test-begin
  (for ((i (in-range 0 256)))
    (for ((j (in-range 0 256)))
      (let ((r (if (zero? (random 2)) i j))
            (g (if (zero? (random 2)) i j))
            (b (if (zero? (random 2)) i j))
            (rr (if (zero? (random 2)) i j))
            (rg (if (zero? (random 2)) i j))
            (rb (if (zero? (random 2)) i j)))
        (check (lambda (x y) (not (xor x y)))
               (equal? (bytes r g b) (bytes rr rg rb))
               (color-equal? (color r g b) (color rr rg rb))))))))

(set! i (image-map (lambda (x) (color (random 256) (random 256) (random 256))) i)) 
          
(cout "Advanced image creation and image reference")
(time
 (test-begin
  (let ((img (make-image 136 120 (lambda (r c) (color r c (+ r c))))))
    (for ((r (in-range 0 136)))
      (for ((c (in-range 0 120)))
        (check = c (image-ref img r c 'green))
        (check = c (image-ref img r c 1))
        (check = r (image-ref img r c 'red))
        (check = r (image-ref img r c 0))
        (check = (+ c r) (image-ref img r c 'blue))
        (check = (+ r c) (image-ref img r c 2)))))
  (for ((clr (image->color-list (make-image 25 35 (color 123 231 98)))))
    (check color-equal? clr (color 123 231 98 12))
    (check (lambda (x y) (not (equal? x y))) clr (color 123 231 98 12)))
  (for ((clr (image->color-list (make-image 25 35))))
    (check color-equal? clr (color 0 0 0 12))
    (check (lambda (x y) (not (equal? x y))) clr (color 0 0 0 12)))))

        
