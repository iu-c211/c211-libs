#lang racket

(require 2htdp/image)

(define band? (or/c 'red 'green 'blue 'alpha 0 1 2 3))

(provide
 (all-from-out 2htdp/image)
 read-image
 write-image 
 (contract-out
  [color-equal? (-> color? color? boolean?)]
  [color-ref    (-> color? band? byte?)]
  [draw-image   (-> image? image?)]
  [image-cols   (-> image? exact-nonnegative-integer?)]
  [image-rows   (-> image? exact-nonnegative-integer?)]
  [image-equal? (-> image? image? boolean?)]
  [image-map    (-> (-> color? color?) image? image?)]
  [image-map-rc (-> (-> exact-nonnegative-integer? exact-nonnegative-integer? color?)
                    image? image?)]
  [image-ref  
   (case->
    (-> image? exact-nonnegative-integer? exact-nonnegative-integer? color?)
    (-> image? exact-nonnegative-integer? exact-nonnegative-integer? band? byte?))]
  [make-image
   (case->
    (-> exact-nonnegative-integer? exact-nonnegative-integer? image?)
    (-> exact-nonnegative-integer? exact-nonnegative-integer?
        (or/c color? (-> exact-nonnegative-integer? exact-nonnegative-integer? color?))
        image?))]
  [list->image  (-> exact-nonnegative-integer? list? image?)]

  [black     color?] [darkgray  color?] [gray      color?] [lightgray color?]
  [white     color?] [red       color?] [green     color?] [blue      color?]
  [yellow    color?] [cyan      color?] [magenta   color?] [orange    color?]
  [pink      color?]
  ))

#|

C211 Image Library for Racket
Seth Kirby

Functions:
(color r g b alpha)
(make-color r g b)
(color? obj)
(image? obj)
(scale num img)
(color-equal? color1 color2)
(color-ref color band)
(draw-image image)
(image-rows image)
(image-cols image)
(image-equal? image image)
(image-map image procedure)
(image-map-rc image procedure)
(image-ref image row column)
(image-ref image row column band)
(make-image rows cols)
(make-image rows cols color)
(make-image rows cols generator)
(list->image cols ls)
(read-image filepath)
(write-image image filename)

--------------------------------------------------------------------------------
Built in from 2htdp/image:
--------------------------------------------------------------------------------

(color r g b alpha)
=> a color

Creates a new color with red, green, and blue values of r, g, and b
respectively, and an alpha transparency of alpha.

--------------------------------------------------------------------------------

(make-color r g b)
=> a color

Creates a new color with red, green, and blue values of r, g, and b respectively.

--------------------------------------------------------------------------------

(color? obj)
=> #t or #f

Test if obj is a color.

--------------------------------------------------------------------------------

(image? obj)
=> #t or #f

Test if obj is a image

--------------------------------------------------------------------------------

(scale num img)
=> image

Returns an image that is img scaled by a factor of num.  (scale 2 img) is twice
as large as image, 0.5 is half as large.

--------------------------------------------------------------------------------
Added by C211 Image Library:
--------------------------------------------------------------------------------

(color-equal? color1 color2)
=> #t or #f

Test if two colors are equal.

|#

(define (color-equal? c1 c2)
  (and (= (color-red c1) (color-red c2))
       (= (color-green c1) (color-green c2))
       (= (color-blue c1) (color-blue c2))
       (= (color-alpha c1) (color-alpha c2))))

#|

--------------------------------------------------------------------------------

(color-ref color band)
=> an integer in the range [0, 255]

band should be one of either the symbols red, green, or blue or the
corresponding integers 0, 1, or 2. Returns the amount
of that specified color as an integer in the range [0, 255] where 0 is none of
that color and 255 is the maximum amount.

|#

(define (color-ref c band)
  (if (integer? band)
      (cond
        [(= band 0) (color-red c)]
        [(= band 1) (color-green c)]
        [(= band 2) (color-blue c)]
        [(= band 3) (color-alpha c)])
      (cond
        [(equal? band 'red) (color-red c)]
        [(equal? band 'green) (color-green c)]
        [(equal? band 'blue) (color-blue c)]
        [(equal? band 'alpha) (color-alpha c)])))

#|

--------------------------------------------------------------------------------

(draw-image image)
=> image

Display the image to the REPL.  This is left for legacy compatibility with old
c211 library code, in racket simply returns the passed image.

|#

(define (draw-image x) x)

#|

--------------------------------------------------------------------------------

(image-rows image)
=> a non-negative integer

Return how many rows are in the image.

|#

(define image-rows image-height)

#|

--------------------------------------------------------------------------------

(image-cols image)
=> a non-negative integer

Return how many columns are in the image.

|#

(define image-cols image-width)

#|

--------------------------------------------------------------------------------

(image-equal? image image)
=> #t or #f

Tests if two images are equal.  Additionally you can use equal?

|#

(define (fast-color-ls-equal? ls1 ls2)
  (if (null? ls1)
      #t
      (if (not (color-equal? (car ls1) (car ls2)))
      #f
      (fast-color-ls-equal? (cdr ls1) (cdr ls2)))))

(define (image-equal? i1 i2)
  (if (not (and (= (image-cols i1) (image-cols i2))
                (= (image-rows i1) (image-rows i2))))
      #f
      (fast-color-ls-equal? (image->color-list i1) (image->color-list i2))))

#|

--------------------------------------------------------------------------------

(image-map procedure image)
=> an image

Create a new image of the same size as image by mapping a procedure of the form
(lambda (color) ...)=>color over each pixel in the original.

|#

(define (image-map func i)
  (color-list->bitmap
   (map func (image->color-list i))
   (image-cols i)
   (image-rows i)))

#|

--------------------------------------------------------------------------------

(image-map-rc procedure image)
=> an image

Create a new image of the same size as image by mapping a procedure of the form
(lambda (r c) ...)=>color over each row and column coordinate in the original.

|#

(define (image-map-rc-helper func ls num-rows num-cols r c)
  (cond
    [(= r num-rows) '()]
    [(= c num-cols)
     (image-map-rc-helper func ls num-rows num-cols (add1 r) 0)]
    [else
     (cons
      (func r c)
      (image-map-rc-helper func (cdr ls) num-rows num-cols r (add1 c)))]))

(define (image-map-rc func i)
  (color-list->bitmap
   (image-map-rc-helper
    func
    (image->color-list i)
    (image-rows i)
    (image-cols i)
    0
    0)
   (image-cols i)
   (image-rows i)))

#|

--------------------------------------------------------------------------------

(image-ref image row column)
(image-ref image row column band)
=> a color or an integer in the range [0, 255]

If band is not specified, access the pixel in image at row x column and return
the corresponding color.

If band is specified and one of either the symbols red, green, or blue or the
corresponding integers 0, 1, or 2, then access the pixel in image at row x column
and return that given band from the pixel as an integer in the range [0, 255].

|#

(define image-ref
  (case-lambda
    [(image row col)
     (list-ref (image->color-list image) (+ (* row (image-cols image)) col))]
    [(image row col band)
     (color-ref
      (list-ref (image->color-list image) (+ (* row (image-cols image)) col))
       (car band))]))

#|



--------------------------------------------------------------------------------

(make-image rows cols)
(make-image rows cols color)
(make-image rows cols generator)
=> an image

Create a new rows x cols image, defaulting to a black background.

If color is specified, fill with that color instead.

If generator is specified, it should be a function of the form
(lambda (r c) ...)=>color where r and c are the coordinates of a specific pixel.

|#

(define make-image
  (case-lambda
    [(rows cols)
     (color-list->bitmap (make-list (* rows cols) black)
                         cols rows)]
    [(rows cols color/f)
     (if (color? color/f)
         (color-list->bitmap (make-list (* rows cols) color/f)
                             cols rows)
         (image-map-rc color/f (make-image rows cols)))]))

#|

--------------------------------------------------------------------------------

(list->image cols ls)
=> an image

Create a new image of width cols from a list of colors

|#

(define (list->image width ls)
  (color-list->bitmap ls width (/ (length ls) width)))

#|

--------------------------------------------------------------------------------

(read-image filepath)
=> an image

Loads and returns a file as an image.  Can take a relative or complete path.
Additionally, copied images may be posted directly into racket.

|#

(define read-image bitmap/file)
    

#|

--------------------------------------------------------------------------------

(write-image image filename)
=> #t or #f

Writes and image to the filename path.  Returns boolean of success of save.

|#

(define write-image save-image)

#|

--------------------------------------------------------------------------------

Color definitions

|#

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

--------------------------------------------------------------------------------

|#