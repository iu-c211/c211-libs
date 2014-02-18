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

(cout "Image mutation and reference")
(time
(test-begin
 (for ((r (in-range 0 (image-rows i))))
   (for ((c (in-range 0 (image-cols i))))
     (for ((band (in-range 0 3)))
       (check = (image-ref i r c band) (image-ref ibl r c band))
       (image-set! i r c band 5)
       (check (lambda (x y) (not (= x y)))
              (image-ref i r c band)
              (image-ref ibl r c band))
       (image-set! ibl r c band 5)
       (check = (image-ref i r c band) (image-ref ibl r c band))
       )))
 (check (lambda (x y) (not (image-equal? x y))) i ibl))
)

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

(cout "Image and color mutation, referencing, and equality testing")
(time
 (test-begin
  (for ((count (in-range 0 2000)))
    (let* ((r (random 50)) (c (random 25)) (clr (image-ref i r c)) (img (image-map (lambda (x) x) i)))
      (let* ((band (random 3)) (newval (random 256)) (oldval (image-ref i r c band)))
        (image-set! i r c band newval)
        (check = oldval (color-ref clr band))
        (check (lambda (x y) (not (xor x y)))
               (color-equal? clr (image-ref i r c))
               (image-equal? img i))
        (color-set! clr band newval)
        (check color-equal? clr (image-ref i r c))))))) 

(cout "Image and list conversions")
(time
 (test-begin
  (for ((times (in-range 0 1000)))
    (let ((r (+ 1 (random 10)))(c (+ 1 (random 10))))
      (let* ((img (make-image r c (color (random 256) (random 256) (random 256))))
             (ls (image->list img)))
        (check = (image-rows img) r)
        (check = (image-cols img) c)
        (check = (length ls) (* c r))
        (define i 0)
        (for ((clr ls))
          (check color-equal? clr (image-ref img (quotient i c) (modulo i c)))
          (set! i (+ i 1)))
        (check image-equal? (list->image c ls) img))))))
          
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
  (for ((clr (image->list (make-image 25 35 (color 123 231 98)))))
    (check color-equal? clr (color-bytes (bytes 12 123 231 98)))
    (check (lambda (x y) (not (equal? x y))) clr (color-bytes (bytes 12 123 231 98))))
  (for ((clr (image->list (make-image 25 35))))
    (check color-equal? clr (color-bytes (bytes 12 0 0 0)))
    (check (lambda (x y) (not (equal? x y))) clr (color-bytes (bytes 12 0 0 0))))))

        
