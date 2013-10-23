#lang racket
(require racket/draw)
(provide (all-defined-out))

(define-struct color (bs) #:mutable
  #:constructor-name color-bytes
  #:omit-define-syntaxes)

(define (color r g b) (color-bytes (bytes 255 r g b)))

(define (color-set! c band val)
  (case band
    [(0 red) (bytes-set! (color-bs c) 1 val)]
    [(1 green) (bytes-set! (color-bs c) 2 val)]
    [(2 blue) (bytes-set! (color-bs c) 3 val)]))

(define (color-ref c band)
  (case band
    [(0 red) (bytes-ref (color-bs c) 1)]
    [(1 green) (bytes-ref (color-bs c) 2)]
    [(2 blue) (bytes-ref (color-bs c) 3)]))

(define (color-equal? c1 c2)
  (and (= (color-ref c1 0) (color-ref c2 0))
       (= (color-ref c1 1) (color-ref c2 1))
       (= (color-ref c1 2) (color-ref c2 2))))

(define (print-color c)
  (display "#<color: ")
  (for ((c (subbytes (color-bs c) 1 4)))
    (display c) (display " "))
  (display ">\n")
  c)

(define (draw-image i) i)

;not safe, image-snip%? isn't loaded 
(define (image? i)
  (or ((is-a?/c bitmap%) i)
      ((is-a?/c bitmap%) (send i get-bitmap))))

(define image-set!
  (case-lambda
    [(img r c clr)
     (define bm
       (if ((is-a?/c bitmap%) img)
           img
           (send img get-bitmap)))
     (send bm set-argb-pixels c r 1 1
           (color-bs clr))]
    [(img r c band value)
     (define bm
       (if ((is-a?/c bitmap%) img)
           img
           (send img get-bitmap)))
     (define bytes (make-bytes 4))
     (send bm get-argb-pixels c r 1 1 bytes)
     (bytes-set! bytes (case band
                         [(0 red) 1]
                         [(1 green) 2]
                         [(2 blue) 3])
                 value)
     (send bm set-argb-pixels c r 1 1 bytes)]))

(define (image-rows i)
  (if ((is-a?/c bitmap%) i)
      (send i get-height)
      (send (send i get-bitmap) get-height)))

(define (image-cols i)
  (if ((is-a?/c bitmap%) i)
      (send i get-width)
      (send (send i get-bitmap) get-width)))

(define (fast-bitmap-equal? bm1 bm2)
  (define h (send bm1 get-height))
  (define w (send bm1 get-width))
  (define bm1bytes (make-bytes (* 4 h w)))
  (define bm2bytes (make-bytes (* 4 h w)))
  (send bm1 get-argb-pixels 0 0 w h bm1bytes)
  (send bm2 get-argb-pixels 0 0 w h bm2bytes)
  (define flag #t)
  (for ((i (in-range 0 (* 4 h w) 4)))
    #:break (and
             (not (and (= (bytes-ref bm1bytes (+ i 1)) (bytes-ref bm2bytes (+ i 1)))
                       (= (bytes-ref bm1bytes (+ i 2)) (bytes-ref bm2bytes (+ i 2)))
                       (= (bytes-ref bm1bytes (+ i 3)) (bytes-ref bm2bytes (+ i 3)))))
             (set! flag #f))
    i)
  flag)

(define (image-equal? i1 i2)
  (define bm1
       (if ((is-a?/c bitmap%) i1)
           i1
           (send i1 get-bitmap)))
  (define bm2
       (if ((is-a?/c bitmap%) i2)
           i2
           (send i2 get-bitmap)))
  (and (= (send bm1 get-height) (send bm2 get-height))
       (= (send bm1 get-width) (send bm2 get-width))
       (fast-bitmap-equal? bm1 bm2)))

(define (image-map func i)
  (define bm
    (if ((is-a?/c bitmap%) i)
        i
        (send i get-bitmap)))
  (define h (send bm get-height))
  (define w (send bm get-width))
  (define b (make-bytes (* 4 h w)))
  (define b2 (make-bytes (* 4 h w)))
  (send bm get-argb-pixels 0 0 w h b)
  (for ((i (in-range 0 (* 4 h w) 4)))
    (bytes-copy!
     b2
     i
     (color-bs (func (color-bytes (subbytes b i (+ i 4)))))
     0
     4))
  (define iout (make-object bitmap% w h))
  (send iout set-argb-pixels 0 0 w h b2)
  iout)

(define image-ref
  (case-lambda
    [(i r c)
     (define bm
       (if ((is-a?/c bitmap%) i)
           i
           (send i get-bitmap)))
     (define b (make-bytes 4))
     (send bm get-argb-pixels c r 1 1 b)
     (color-bytes (subbytes b 0 4))]
    [(i r c band)
     (define bm
       (if ((is-a?/c bitmap%) i)
           i
           (send i get-bitmap)))
     (define b (make-bytes 4))
     (send bm get-argb-pixels c r 1 1 b)
     (bytes-ref b (case band
                    [(0 red) 1]
                    [(1 green) 2]
                    [(2 blue) 3]))]))

(define make-image
  (case-lambda
    [(r c)
     (define b (make-bytes (* 4 r c)))
     (define blk (bytes 255 0 0 0))
     (for ((i (in-range 0 (* 4 r c) 4)))
       (bytes-copy! b i blk 0 4))
     (define bm (make-object bitmap% c r))
     (send bm set-argb-pixels 0 0 c r b)
     bm]
    [(r c f/color)
     (cond
       [(procedure? f/color)
        (define b (make-bytes (* 4 r c)))
        (for ((rr (in-range 0 r)))
          (for ((rc (in-range 0 c)))
            (bytes-copy!
             b
             (+ (* 4 (* rr c)) (* 4 rc))
             (color-bs (f/color rr rc))
             0
             4)))
        (define bm (make-object bitmap% c r))
        (send bm set-argb-pixels 0 0 c r b)
        bm]
       [else (define b (make-bytes (* 4 r c)))
             (define clr (color-bs f/color))
             (for ((i (in-range 0 (* 4 r c) 4)))
               (bytes-copy! b i clr 0 4))
             (define bm (make-object bitmap% c r))
             (send bm set-argb-pixels 0 0 c r b)
             bm])]))

(define (list->image c ls)
  (define r (quotient (length ls) c))
  (define b (make-bytes (* 4 r c)))
  (for ((clr ls)(i (in-range 0 (* 4 r c) 4)))
    (bytes-copy! b i (color-bs clr) 0 4))
  (define bm (make-object bitmap% c r))
  (send bm set-argb-pixels 0 0 c r b)
  bm)

(define (image->list img)
  (define bm
       (if ((is-a?/c bitmap%) img)
           img
           (send img get-bitmap)))
  (define h (image-rows bm))
  (define w (image-cols bm))
  (define bs (make-bytes (* 4 h w)))
  (send bm get-argb-pixels 0 0 w h bs)
  (for/list ((i (in-range 0 (* 4 w h) 4)))
    (color (bytes-ref bs (+ i 1))
           (bytes-ref bs (+ i 2))
           (bytes-ref bs (+ i 3)))))

(define read-image 1)
(define write-image 1)

(define black     (color 0 0 0))
(define darkgray  (color 84 84 84))
(define gray      (color 192 192 192))
(define lightgray (color 205 205 205))
(define white     (color 255 255 255))
(define red       (color 255 0 0))
(define green     (color 0 255 0))
(define blue      (color 0 0 255))
(define yellow    (color 255 255 0))
(define cyan      (color 0 255 255))
(define magenta   (color 255 0 255))
(define orange    (color 255 127 0))
(define pink      (color 188 143 143))   

#|
(for ((r (in-range 0 100)))
  (for ((c (in-range 0 100)))
    (for ((v (in-range 0 3)))
    (image-set! i2 r c v 125))))
|
|#

#|
(define h (image-height i))
(define w (image-width i))
(define bytes (make-bytes (* 4 h w)))
(define old-bytes (send bm get-argb-pixels 0 0 h w bytes))

(define (image-rc func img)
  (define h (image-height img))
  (define w (image-width img))
  (define bytes (make-bytes (* 4 h w)))
  (define bm (send i get-bitmap))
  (define old-bytes (send bm get-argb-pixels 0 0 h w bytes))
|#

#|
(define (color-ref color band)
  (case band
    [(0 red)   (color-r color)]
    [(1 green) (color-g color)]
    [(2 blue)  (color-b color)]
    [(3 alpha)  (color-a color)]))
|#

#|
(define image-set!
  (case-lambda
    [(img r c clr)
     (define bm (send img get-bitmap))
     (send bm set-argb-pixels c r 1 1
           (bytes (color-ref clr 3)
                  (color-ref clr 0)
                  (color-ref clr 1)
                  (color-ref clr 2)))]
    [(img r c band value)
     (define bm (send img get-bitmap))
     (define bytes (make-bytes 4))
     (send bm get-argb-pixels c r 1 1 bytes)
     (define ref (case band
                   [(0 red)   1]
                   [(1 green) 2]
                   [(2 blue)  3]
                   [(3 alpha) 0]))
     (bytes-set! bytes ref value)
     (send bm set-argb-pixels c r 1 1 bytes)]))

(for ((r (in-range 0 100)))
  (for ((c (in-range 0 100)))
    (for ((v (in-range 0 3)))
    (image-set! i r c v 60))))

|#