c211-lib
========

A collection of libraries designed for the C211 course at IU

C211 Image Library for Racket

Functions:

```Racket
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
```

--------------------------------------------------------------------------------
Built in from 2htdp/image:
--------------------------------------------------------------------------------

```Racket
(color r g b alpha)
=> a color
```

Creates a new color with red, green, and blue values of r, g, and b
respectively, and an alpha transparency of alpha.

--------------------------------------------------------------------------------

```Racket
(make-color r g b)
=> a color
```

Creates a new color with red, green, and blue values of r, g, and b respectively.

--------------------------------------------------------------------------------

```Racket
(color? obj)
=> #t or #f
```

Test if obj is a color.

--------------------------------------------------------------------------------

```Racket
(image? obj)
=> #t or #f
```

Test if obj is a image

--------------------------------------------------------------------------------

```Racket
(scale num img)
=> image
```

Returns an image that is img scaled by a factor of num.  (scale 2 img) is twice
as large as image, 0.5 is half as large.

--------------------------------------------------------------------------------
Added by C211 Image Library:
--------------------------------------------------------------------------------

```Racket
(color-equal? color1 color2)
=> #t or #f
```

Test if two colors are equal.

```Racket
(define (color-equal? c1 c2)
  (and (= (color-red c1) (color-red c2))
       (= (color-green c1) (color-green c2))
       (= (color-blue c1) (color-blue c2))
       (= (color-alpha c1) (color-alpha c2))))
```

--------------------------------------------------------------------------------

```Racket
(color-ref color band)
=> an integer in the range [0, 255]
```

Band should be one of either the symbols red, green, or blue or the
corresponding integers 0, 1, or 2. Returns the amount
of that specified color as an integer in the range [0, 255] where 0 is none of
that color and 255 is the maximum amount.

```Racket
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
```

--------------------------------------------------------------------------------

```Racket
(draw-image image)
=> image
```

Display the image to the REPL.  This is left for legacy compatibility with old
c211 library code, in racket simply returns the passed image.

```Racket
(define (draw-image x) x)
```

--------------------------------------------------------------------------------

```Racket
(image-rows image)
=> a non-negative integer
```

Return how many rows are in the image.

```Racket
(define image-rows image-height)
```

--------------------------------------------------------------------------------

```Racket
(image-cols image)
=> a non-negative integer
```

Return how many columns are in the image.

```Racket
(define image-cols image-width)
```

--------------------------------------------------------------------------------

```Racket
(image-equal? image image)
=> #t or #f
```

Tests if two images are equal.  Additionally you can use equal?

```Racket
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
```

--------------------------------------------------------------------------------

```Racket
(image-map procedure image)
=> an image
```

Create a new image of the same size as image by mapping a procedure of the form
(lambda (color) ...)=>color over each pixel in the original.

```Racket
(define (image-map func i)
  (color-list->bitmap
   (map func (image->color-list i))
   (image-cols i)
   (image-rows i)))
```

--------------------------------------------------------------------------------

```Racket
(image-map-rc procedure image)
=> an image
```

Create a new image of the same size as image by mapping a procedure of the form
(lambda (r c) ...)=>color over each row and column coordinate in the original.

```Racket
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
```

--------------------------------------------------------------------------------

```Racket
(image-ref image row column)
(image-ref image row column band)
=> a color or an integer in the range [0, 255]
```

If band is not specified, access the pixel in image at row x column and return
the corresponding color.

If band is specified and one of either the symbols red, green, or blue or the
corresponding integers 0, 1, or 2, then access the pixel in image at row x column
and return that given band from the pixel as an integer in the range [0, 255].

```Racket
(define image-ref
  (case-lambda
    [(image row col)
     (list-ref (image->color-list image) (+ (* row (image-cols image)) col))]
    [(image row col band)
     (color-ref
      (list-ref (image->color-list image) (+ (* row (image-cols image)) col))
       (car band))]))
```

--------------------------------------------------------------------------------

```Racket
(make-image rows cols)
(make-image rows cols color)
(make-image rows cols generator)
=> an image
```

Create a new rows x cols image, defaulting to a black background.

If color is specified, fill with that color instead.

If generator is specified, it should be a function of the form
(lambda (r c) ...)=>color where r and c are the coordinates of a specific pixel.

```Racket
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
```

--------------------------------------------------------------------------------

```Racket
(list->image cols ls)
=> an image
```

Create a new image of width cols from a list of colors

```Racket
(define (list->image width ls)
  (color-list->bitmap ls width (/ (length ls) width)))
```

--------------------------------------------------------------------------------

```Racket
(read-image filepath)
=> an image
```

Loads and returns a file as an image.  Can take a relative or complete path.
Additionally, copied images may be posted directly into racket.

```Racket
(define read-image bitmap/file)
```

--------------------------------------------------------------------------------

```Racket
(write-image image filename)
=> #t or #f
```

Writes and image to the filename path.  Returns boolean of success of save.

```Racket
(define write-image save-image)
```

--------------------------------------------------------------------------------

Color definitions

```Racket
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
```

--------------------------------------------------------------------------------
