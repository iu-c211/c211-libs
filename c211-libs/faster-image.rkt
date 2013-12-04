#lang racket

; (define-syntax import (syntax-rules () [(_ * ...) (void)]))

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  color procs
;  color color-ref color? color->string string->color 
;  color-equal?

(define-struct clr (red green blue))
(define color? clr?)

(define color
  (case-lambda
   [(r g b)
    (define legal?
      (lambda (x)
        (and (integer? x) (<= 0 x 255))))
    (if (legal? r) 
        (if (legal? g) 
            (if (legal? b)
                (make-clr (inexact->exact r)
                          (inexact->exact g)
                          (inexact->exact b))
                (error 'color (format "illegal blue value: ~s" b)))
            (error 'color (format "illegal green value: ~s" g)))
        (error 'color (format "illegal red value: ~s" r)))]
   [(s) (if (hexcode? s)
            (color (string->number (substring s 0 2) 16)
                   (string->number (substring s 2 4) 16)
                   (string->number (substring s 4 6) 16))
            (error 'color (format "~s is not a hexcode color" s)))]))


(define black     (color   0   0   0))
(define white     (color 255 255 255))
(define red       (color 255   0   0))
(define blue      (color   0   0 255))
(define cyan      (color   0 255 255))
(define gray      (color 192 192 192))
(define darkgray  (color  84  84  84))
(define lightgray (color 205 205 205))
(define green     (color   0 255   0))
(define magenta   (color 255   0 255))
(define orange    (color 255 127   0))
(define pink      (color 188 143 143))
(define yellow    (color 255 255   0))

(define color-ref
  (lambda (color sym)
    (when (not (color? color))
        (error 'color-ref (format "~s is not a color" color)))    
    (case sym
      [(red) (clr-red color)]
      [(0) (clr-red color)]
      [(green) (clr-green color)]
      [(1) (clr-green color)]
      [(blue) (clr-blue color)]
      [(2) (clr-blue color)]
      [else (error 'color-ref (format "unknown symbol: ~s" sym))])))

(define color->hex
  (lambda (color)
    (when (not (color? color))
        (error 'color->hex (format "~s is not a color" color)))    
    (format "~6,,,'0@a"
            (number->string (+ (* (expt 16 4) (color-ref color 'red))
                               (* (expt 16 2) (color-ref color 'green))
                               (color-ref color 'blue)) 16))))

(define hexcode?
  (lambda (s)
    (and (string? s)
         (= 6 (string-length s))
         (let loop ([i 0])
           (or (= i 6)
               (and (member (string-ref s i) 
                            (string->list "0123456789abcdefABCDEF"))
                    (loop (+ i 1))))))))

(define hex->color
  (lambda (s)
    (if (hexcode? s)
        (color (string->number (substring s 0 2) 16)
               (string->number (substring s 2 4) 16)
               (string->number (substring s 4 6) 16))
        (error 'hex->color (format "~s is not a hexcode color" s)))))

; color-equal? takes two colors, c1 and c2, and returns #t if and only
; if the colors are logically equivalent, and #f otherwise

(define color-equal?
  (lambda (c1 c2)
    (and (= (color-ref c1 'red) (color-ref c2 'red))
         (= (color-ref c1 'green) (color-ref c2 'green))
         (= (color-ref c1 'blue) (color-ref c2 'blue)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; image procs
;  make-image image-ref image-set! image-rows image-cols
;  read-image write-image draw-image 
;  image-equal? image?

(define make-image
  (lambda (rows cols . args)
    (let ([gen-proc
           (cond
            [(null? args) (lambda (i j) black)]
            [(not (null? (cdr args)))
             (error 'make-image "too many arguments")]
            [(color? (car args)) (lambda (i j) (car args))]
            [(procedure? (car args)) (car args)]
            [else (error 'make-image (format "unknown fill: ~s" 
                                             (car args)))])])
      (let ([img (make-vector rows)])
        (let loop ([i 0])
          (when (< i rows)
            (vector-set! img i 
                         (let ([row (make-vector cols)])
                           (let loop ([j 0])
                             (when (< j cols)
                               (vector-set! row j (gen-proc i j))
                               (loop (+ j 1))))
                           row))
            (loop (+ i 1))))
        img))))

(define image-ref
  (lambda (img i j)
    (unless (image? img 0 0)
        (error 'image-ref "first argument is not an image"))
    (if (and (integer? i) (<= 0 i (- (image-rows img) 1)))
        (if (and (integer? j) (<= 0 j (- (image-cols img) 1)))
            (vector-ref (vector-ref img i) j)
            (error 'image-ref (format "~a is an illegal index" j)))
        (error 'image-ref (format "~a is an illegal index" i)))))

(define image-set!
  (lambda (img i j x)
    (unless (image? img 0 0)
        (error 'image-ref "first argument is not an image"))
    (if (and (integer? i) (<= 0 i (- (image-rows img) 1)))
        (if (and (integer? j) (<= 0 j (- (image-cols img) 1)))
            (if (color? x)
                (vector-set! (vector-ref img i) j x)
                (error 'image-set! (format "~a is not a color" x)))
            (error 'image-set! (format "~a is an illegal index" j)))
        (error 'image-set! (format "~a is an illegal index" i)))))

(define image-rows
  (lambda (img)
    (cond
     [(image? img 1 1) (vector-length img)]
     [else
      (error 'image-rows (format "~a is not an image" img))])))

(define image-cols
  (lambda (img)
    (cond
     [(image? img 1 1) (if (zero? (vector-length img))
                           0
                           (vector-length (vector-ref img 0)))]
     [else
      (error 'image-cols (format "~a is not an image" img))])))

(define image-map
  (lambda (f img)
    (unless (image? img 5 5)
        (error 'image-ref "second argument is not an image"))
    (let* ([rows (image-rows img)]
           [cols (image-cols img)]
           [img-out (make-image rows cols)])
      (let loop ([r 0])
        (when (< r rows)
          (let loop ([c 0])
            (when (< c cols)
              (image-set! img-out r c (f (image-ref img r c)))
              (loop (+ c 1))))
          (loop (+ r 1))))
      img-out)))

(define open-binary-input-file #f)
(define open-binary-output-file #f)
(define get-byte #f)
(define put-byte #f)
(define peek-byte #f)
(define read-jpeg-image #f)
(define read-gif-image #f)
(define write-gif-image #f)
(define shift-left #f)
(define shift-right #f)
(define logn #f)
(define shift #f)

(define ends-with-ci?
  (lambda (s t)
    (let ([n (string-length s)]
          [m (string-length t)])
      (and (<= m n)
           (string-ci=? (substring s (- n m) n)
                        t)))))

#;
(define v8?
  (lambda ()
    (top-level-bound? 'open-file-input-port)))

  
(define read-ppm-image
  (lambda (filename)
    (define skip-whitespace
      (lambda (p)
        (let loop ()
          (let ([b (peek-byte p)])
            (when (not (eof-object? b))
                (let ([c (integer->char b)])
                  (cond
                   [(char-whitespace? c) (get-byte p) (loop)]
                   [(char=? c #\#) (skip-line p) (loop)]
                   [else (void)])))))))
    
    (define skip-line
      (lambda (p)
        (let loop ()
          (let ([b (get-byte p)])
            (unless (eol? b)
                (loop))))))

    (define eol?  ;; (char->integer #\newline) ==> 10
      (lambda (b)
        (or (eof-object? b) (= b 10))))

    (define read-int
      (lambda (p)
        (skip-whitespace p)
        (let loop ([x 0])
          (let ([b (peek-byte p)])
            (if (eof-object? b)
                x
                (let ([c (integer->char b)])
                  (if (char-whitespace? c)
                      x
                      (begin
                        (get-byte p)
                        (loop (+ (* x 10)
                                 (- b (char->integer #\0))))))))))))

    (define read-header
      (lambda (p)
        (let* ([b0 (get-byte p)]
               [b1 (get-byte p)])
          (if (or (eof-object? b0) (eof-object? b1))
              ""
              (apply string 
                     (map integer->char `(,b0 ,b1)))))))

    (let ([p (open-binary-input-file filename)])
      (let ([head (read-header p)])
        (when (and (not (string-ci=? "P5" head))
                   (not (string-ci=? "P6" head)))
            (error 'read-image "Not a binary PPM file"))
        (skip-whitespace p)		; Throw away newline and comments.
        (let* ([cols (read-int p)]    	; Get image dimensions.
               [rows (read-int p)]
               [image (make-image rows cols)])
          (read-int p)  ; Throw away maximum color component
          (skip-line p) ; Throw away newline.
          (let rloop ([r 0])
            (when (< r rows)
              (let cloop ([c 0])
                (when (< c cols)
                  (if (string-ci=? head "P5")
                      (let ([x (get-byte p)])
                        (image-set! image r c (color x x x)))
                      (let* ([red (get-byte p)]
                             [green (get-byte p)]
                             [blue (get-byte p)])
                        (image-set! image r c (color red green blue))))
                  (cloop (+ c 1))))
              (rloop (+ r 1))))
          (close-input-port p)
          image)))))


(define draw-image
  (lambda args
    (void)))

(define image-equal?
  (lambda (img1 img2)
    (let ([rows (image-rows img1)]
          [cols (image-cols img1)])
      (and (= rows (image-rows img2))
           (= cols (image-cols img2))
           (let loop ([r 0])
             (or (= r rows)
                 (and (let loop ([c 0])
                        (or (= c cols)
                            (and (color-equal? (image-ref img1 r c)
                                               (image-ref img2 r c))
                                 (loop (+ c 1)))))
                      (loop (+ r 1)))))))))

; accepts two optional arguments representing a rectangular region
; in the upper left; the color checks are then restricted to that region
; e.g., (image? img 10 10)
(define image?
  (lambda (img . args)
    (and 
         (vector? img)
         (or (zero? (vector-length img))
             (vector? (vector-ref img 0)))
         (let ([rows (vector-length img)]
               [cols (vector-length (vector-ref img 0))])
           (let ([rmax (min rows (if (null? args) rows (car args)))]
                 [cmax (min cols (if (or (null? args) (null? (cdr args)))
                                     cols 
                                     (cadr args)))])
             (let loop ([r 0])
               (or (= r rmax)
                   (and 
                    (vector? (vector-ref img r))
                    (= cols (vector-length (vector-ref img r)))
                    (let loop ([c 0])
                          (or (= c cmax)
                              (and 
                               (let ([colr (vector-ref (vector-ref img r) c)])
                                 (if (color? colr)
                                     (loop (+ c 1))
                                      #f)))))
                    (loop (+ r 1))))))))))

(define image->list
  (lambda (image)
    (let loop ([r (sub1 (image-rows image))] [acc '()])
      (if (negative? r)
          acc
          (loop (- r 1)
            (let loop ([c (sub1 (image-cols image))] [acc acc])
              (if (negative? c)
                  acc
                  (loop (- c 1) (cons (image-ref image r c) acc)))))))))

(define list->image
  (lambda (num-cols ls)
    (make-image (quotient (length ls) num-cols) num-cols
      (lambda (r c n m)
        (list-ref ls (+ (* r num-cols) c))))))

(define $obama$ (make-image 123 456))

(define read-image
    (lambda (path)
      path))




;;   Commented out for autograder
#;
(define read-image
  (let ()
    (if (v8?) 
        (begin ; ChezScheme Version 8
          (set! open-binary-input-file (lambda (fn) (open-file-input-port fn)))
          (set! get-byte (lambda (p) (get-u8 p)))
          (set! peek-byte (lambda (p) (lookahead-u8 p)))
           
          (set! shift-left bitwise-arithmetic-shift-left)
          (set! shift-right bitwise-arithmetic-shift-right)
          (set! shift bitwise-arithmetic-shift))
        (begin ; ChezScheme Version 7
          (set! open-binary-input-file (lambda (fn) (open-input-file fn)))
          (set! get-byte (lambda (p) 
                           (let ([x (read-char p)])
                             (if (eof-object? x)
                                 x
                                 (char->integer x)))))
          (set! peek-byte (lambda (p) (char->integer (peek-char p))))
          (set! shift-left (lambda (x n) (* x (expt 2 n))))
          (set! shift-right (lambda (x n) (quotient x (expt 2 n))))
          (set! shift (lambda (x n) (if (negative? n)
                                        (shift-right x (- n))
                                        (shift-left x n))))
          ))
    (lambda (filename)
      (cond
       [(ends-with-ci? filename ".ppm") (read-ppm-image filename)]
       [(ends-with-ci? filename ".gif") (read-gif-image filename)]
       [(or (ends-with-ci? filename ".jpg")
            (ends-with-ci? filename ".jpeg"))
        (read-jpeg-image filename)]
       [else
        (error 'read-image
               (format "Unrecognized file format: ~s" filename))]))))
#;
(define write-image
  (let ()
    (if (v8?)
        (begin ; ChezScheme Version 8
          (set! open-binary-output-file 
                (lambda (fn) 
                  (open-file-output-port fn (file-options no-fail))))
          (set! put-byte (lambda (b p) (put-u8 p b)))
          (set! shift-left bitwise-arithmetic-shift-left)
          (set! shift-right bitwise-arithmetic-shift-right)
          (set! logn log)
          (set! shift bitwise-arithmetic-shift))
      
        (begin ; ChezScheme Version 7
          (set! open-binary-output-file 
                (lambda (fn) (open-output-file fn 'replace)))
          (set! put-byte (lambda (b p) (fprintf p "~a" (integer->char b))))
          (set! shift-left (lambda (x n) (* x (expt 2 n))))
          (set! shift-right (lambda (x n) (quotient x (expt 2 n))))
          (set! logn (lambda (num base) (/ (log num) (log base))))
          (set! shift (lambda (x n) (if (negative? n)
                                        (shift-right x (- n))
                                        (shift-left x n))))
          ))
    (lambda (image filename)
      (cond
       [(ends-with-ci? filename ".ppm")
        (write-ppm-image image filename)]
       [(ends-with-ci? filename ".gif")
        (write-gif-image image filename)]
       [else
        (error 'write-image "file format not supported: use ppm or gif")]))))
#;
(define write-ppm-image
  (let ()
    (define put-string
      (lambda (s p)
        (for-each (lambda (c)
                    (put-byte (char->integer c) p))
                  (string->list s))))
    
    (lambda (image filename)    
      (let ([p (open-binary-output-file filename)]
            [rows (image-rows image)]
            [cols (image-cols image)])
        (put-string (format "P6~n") p)
        (put-string (format "# ~a~n" filename) p)
        (put-string (format "~a ~a~n" (image-cols image) (image-rows image)) p)
        (put-string (format "255~n") p) ; Assume the maximum RGB value is 255
        (let rloop ([r 0])
          (when (< r rows)
            (let cloop ([c 0])
              (when (< c cols)
                (let ([rgb (image-ref image r c)])
                  (let ([red (integer->char (color-ref rgb 'red))]
                        [green (integer->char (color-ref rgb 'green))]
                        [blue (integer->char (color-ref rgb 'blue))])
                    (put-string (format "~a~a~a" red green blue) p)))
                (cloop (+ c 1))))
            (rloop (+ r 1))))
        (close-output-port p)))))  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Tessa's gif reader and writer

#|
(let () ;; start gif let

  ;; gif constants
  
  (define MAX_DIC_SIZE 4096)
  (define MAX_NUM_COLORS 256)
  (define MAX_CODE_SIZE 12)
  (define MAX_TABLE_NUM 7)
  (define MAX_CODE 4095) 
  (define IMAGE_DATA_START 44)
  (define TRAILER 59)
  (define EXT_MARKER 33)
  (define GRAPHICS_EXT_MARKER 249)
  
  ;; gif reader data vector indices
    
  (define IDX_LIST_IDX 0)
  (define CODE_SIZE_IDX_R 1)
  (define BYTE_I_IDX_R 2)
  (define BYTE_IDX_R 3)
  (define COUNTER_IDX 4)
  (define NUM_BYTES_IDX 5)
  (define CODE_IDX 6)
  (define OLD_IDX 7)
  
  ;; gif reader data vector indices 
  
  ;; buffer-vec
  
  (define TBL_IDX_IDX 0)
  (define BUFFER_IDX 1)
  (define DIC_IDX 2)
  (define BUFFER&IDX_IDX 3)
  
  ;; info-vec
  
  (define BYTE_I_IDX_W 0)
  (define BYTE_IDX_W 1)
  (define OUTPUT_I_IDX 2)
  (define CODE_SIZE_IDX_W 3) 
  
  (define NUM_CLRS_IDX 0)
  (define CLR_VEC_IDX 1)
  
  ;; color table
  
  (define CLR_IDX 0)
  (define CLR_IDX_IDX 1)
  
  ;; dictionary
  
  (define TRANS_IDX 0)
  (define DIC_CODE_IDX 1)
  (define LENGTH_IDX 1)
  
  ;;;;;;;;;;;;;;
  ;; octtrees ;;
  ;;;;;;;;;;;;;;
  
  ;; An octtree is a tree with 8 subtrees, each of which is also an
  ;; octtree.  Octrees are represented as 8 element vectors. An 
  ;; octtree with multiple levels consistes of nested vectors. 
  ;; There is one 8 element vector for each subtree in the 
  ;; level. 
  
  ;; The following records are defined for an octree node and for 
  ;; an empty octtree.
  
  (define-record octnode (data tr0 tr1 tr2 tr3 tr4 tr5 tr6 tr7))
  (define-record octempty ())
  
  ;; empty-octtree makes an empty octtree.
  
  (define empty-octtree
    (lambda ()
      (make-octempty)))
  
  ;; empty-octtree? takes an object, obj, and returns true if and only
  ;; if obj is the empty octtree.
  
  (define empty-octtree?
    (lambda (obj)
      (octempty? obj)))
  
  ;; octtree takes a root value and 8 subtrees, and returns an octtree.
  
  (define octtree
    (lambda (data tr0 tr1 tr2 tr3 tr4 tr5 tr6 tr7)
      (make-octnode data tr0 tr1 tr2 tr3 tr4 tr5 tr6 tr7)))
  
  ;; octtree? takes an object, obj, and returns true if and only if obj 
  ;; is an octtree.
  
  (define octtree?
    (lambda (obj)
      (or (empty-octtree? obj)
          (and (oct-node? obj)
               (octtree? (subtree0 obj))
               (octtree? (subtree1 obj))
               (octtree? (subtree2 obj))
               (octtree? (subtree3 obj))
               (octtree? (subtree4 obj))
               (octtree? (subtree5 obj))
               (octtree? (subtree6 obj))
               (octtree? (subtree7 obj))))))
  
  ;; oct-root-value takes an octtree and returns its root value.
  
  (define oct-root-value
    (lambda (tr)
      (octnode-data tr)))
  
  ;; subtree0 takes an octtree and returns its first subtree.
  
  (define subtree0
    (lambda (tr)
      (octnode-tr0 tr)))
  
  ;; subtree1 takes an octtree and returns its second subtree.
  
  (define subtree1
    (lambda (tr)
      (octnode-tr1 tr)))
  
  ;; subtree2 takes an octtree and returns its third subtree.
  
  (define subtree2
    (lambda (tr)
      (octnode-tr2 tr)))
  
  ;; subtree3 takes an octtree and returns its fourth subtree.
  
  (define subtree3
    (lambda (tr)
      (octnode-tr3 tr)))
  
  ;; subtree4 takes an octtree and returns its fifth subtree.
  
  (define subtree4
    (lambda (tr)
      (octnode-tr4 tr)))
  
  ;; subtree5 takes an octtree and returns its sixth subtree.
  
  (define subtree5
    (lambda (tr)
      (octnode-tr5 tr)))
  
  ;; subtree6 takes an octtree and returns its seventh subtree.
  
  (define subtree6
    (lambda (tr)
      (octnode-tr6 tr)))
  
  ;; subtree7 takes an octtree and returns its eighth subtree.
  
  (define subtree7
    (lambda (tr)
      (octnode-tr7 tr)))
  
  ;; subtreei takes an octtree, tr, and a number, i and returns 
  ;; the ith subtree of tr.
  
  (define subtreei
    (lambda (tr i)
      (cond
       [(= i 0) (subtree0 tr)]
       [(= i 1) (subtree1 tr)]
       [(= i 2) (subtree2 tr)]
       [(= i 3) (subtree3 tr)]
       [(= i 4) (subtree4 tr)]
       [(= i 5) (subtree5 tr)]
       [(= i 6) (subtree6 tr)]
       [else (subtree7 tr)])))
  
  ;; oct-leaf takes an object, obj and returns a leaf whose root value
  ;; is obj.
  
  (define octleaf
    (lambda (obj)
      (octtree obj 
               (empty-octtree) (empty-octtree)
               (empty-octtree) (empty-octtree)
               (empty-octtree) (empty-octtree)
               (empty-octtree) (empty-octtree))))
  
  ;; oct-leaf? takes an object, obj returns true if and only if obj is
  ;; a leaf.
  
  (define octleaf?
    (lambda (obj)
      (and (octnode? obj)
           (empty-octtree? (subtree0 obj))
           (empty-octtree? (subtree1 obj))
           (empty-octtree? (subtree2 obj))
           (empty-octtree? (subtree3 obj))
           (empty-octtree? (subtree4 obj))
           (empty-octtree? (subtree5 obj))
           (empty-octtree? (subtree6 obj))
           (empty-octtree? (subtree7 obj)))))
  
  ;; set-subtreei! takes an octtree octtr, a number, i, between 0 and 7,
  ;; and tree, tr.  The octtree is mutated by changing its ith 
  ;; subtree to tr.
  
  (define set-subtreei!
    (lambda (octtr i tr)
      (cond
       [(= i 0) (set-octnode-tr0! octtr tr)]
       [(= i 1) (set-octnode-tr1! octtr tr)]
       [(= i 2) (set-octnode-tr2! octtr tr)]
       [(= i 3) (set-octnode-tr3! octtr tr)]
       [(= i 4) (set-octnode-tr4! octtr tr)]
       [(= i 5) (set-octnode-tr5! octtr tr)]
       [(= i 6) (set-octnode-tr6! octtr tr)]
       [(= i 7) (set-octnode-tr6! octtr tr)])))
  
  ;; add-to-tree! takes an octtree, vec-tr, a color clr, and number,
  ;; num.  Mutates the tree by adding a vector containg the color and
  ;; number to the tree. The vector is added in the 8th level of the
  ;; tree.
  
  (define add-to-tree!
    (lambda (vec-tr clr num)
      (let loop ([vec vec-tr]
                 [byte-i 7])
        (let* ([subtree-num 
                (color->oct-index clr byte-i)])
          (cond
           [(zero? (vector-length (vector-ref vec subtree-num)))
            (vector-set! vec subtree-num (make-vector 8 (vector)))
            (loop vec byte-i)]
           [(= byte-i 0) 
            (vector-set! vec subtree-num (vector clr num))]
           [else 
            (loop (vector-ref vec subtree-num)  (sub1 byte-i))])))))
  
  ;; table->octtree takes a color table, clr-table, and returns an 
  ;; octtree containing all of the colors in the table.
  
  (define table->octtree
    (lambda (clr-table num-clrs)
      (let ([size (vector-length clr-table)]
            [octtr (make-vector 8 (vector))])
        (let loop ([i 0])
          (cond
           [(or (>= i num-clrs) (>= i size)) octtr]
           [else
            (let ([clr (vector-ref(vector-ref clr-table i) CLR_IDX)])
              (add-to-tree! octtr clr i)
              (loop (add1 i)))])))))

  ;; find-octtree-match takes an octtree, vec-tr and a color, clr.
  ;; Returns the color in vec-tr that is the closest color to clr.
  
  (define find-octtree-match
    (lambda (vec-tr clr)
      (let ([subtree-num0 
             (color->oct-index clr 7)])
        (let loop ([vec (vector-ref vec-tr subtree-num0)]
                   [byte-i 6])
          (let ([subtree-num  
                 (find-non-zero vec (color->oct-index clr byte-i))]) 
            (cond
             [(=  2 (vector-length  (vector-ref vec subtree-num)))
              (vector-ref (vector-ref vec subtree-num) CLR_IDX)]
             [else 
              (loop (vector-ref vec subtree-num)(sub1 byte-i))]))))))
  
  ;; color-find-octtree takes an octtree, vec-tr, and a color clr.
  ;; Finds the color by traversing the tree to a leaf (the one that
  ;; contains the color).  Returns the color's index.
  
  (define color-find-octtree
    (lambda (vec-tr clr)
      (let ([subtree-num0 
             (color->oct-index clr 7)])
        (let loop ([vec (vector-ref vec-tr subtree-num0)]
                   [byte-i 6])
          (let ([subtree-num  
                 (color->oct-index clr byte-i)]) 
            (cond
             [(=  2 (vector-length  (vector-ref vec subtree-num)))
              (vector-ref (vector-ref vec subtree-num) CLR_IDX_IDX)]
             [else 
              (loop (vector-ref vec subtree-num)(sub1 byte-i))]))))))
  
  ;; color->oct-index takes a color, clr, and a byte index, byte-i.
  ;; Returns the corresponsing octtree index, subtree number. 
  
  (define color->oct-index
    (lambda (clr byte-i)
      (+ (shift-left (shift-right 
                      (logand (color-ref clr 'red) 
                              (shift-left 1 byte-i)) byte-i) 2)
         (shift-left(shift-right 
                     (logand (color-ref clr 'green) 
                             (shift-left 1 byte-i)) byte-i) 1)
         (shift-right 
          (logand (color-ref clr 'blue) (shift-left 1 byte-i)) byte-i))))
  
  ;; find-non-zero takes a vector, vec, and an index, idx. Finds
  ;; the first not empty vector in vec starting at indx.  Returns
  ;; its index.  Assume that the vector contains at lest one
  ;; element that is not the empty vector.
  
  (define find-non-zero
    (lambda (vec idx)
      (let loop ([i idx])
        (cond 
         [(color? (vector-ref vec i)) i]
         [(zero? (vector-length (vector-ref vec i)))
          (loop (modulo (add1 i) 8))]
         [else i]))))
  
  
  ;; read-two-byte-number takes an inport port. Reads two bytes 
  ;; (numbers between 0 and 255) and converts them into a number.
  ;;  (ex 10 0 => 10     
  ;;      0 1 => 256)
  
  (define read-two-byte-number
    (lambda (p)
      (let* ([b1 (get-byte p)]
             [b2 (get-byte p)])
        (logor (shift-left b2 8) b1))))

  ;; read-color takes an input port p, and reads three bytes.
  ;; Converts the bytes to a color and returns the
  ;; color.
  
  (define read-color
    (lambda (p)
      (let* ([red (get-byte p)]
             [green (get-byte p)]
             [blue (get-byte p)])
        (color red green blue))))
  
  ;; write-color takes a color,clr, and an output port, p. Writes
  ;; the color's red, green, and blue components to the output port.
  
  (define write-color
    (lambda (clr p)
      (put-byte (color-ref clr 'red) p)
      (put-byte (color-ref clr 'green) p)
      (put-byte (color-ref clr 'blue) p)))
  
  ;; read-color-table takes an input port, p, and a size and returns a
  ;; color table, a vector of colors, of the given size.
  
  (define read-color-table
    (lambda (p size)
      (let ([table (make-vector size black)])
        (let loop ([i 0])
          (when (< i size)
            (vector-set! table i (read-color p))
            (loop (add1 i))))
        table)))
  
  ;; read-image-gif takes a gif file, converts it into an image, and 
  ;; returns the image.

  (define read-image-gif
    (lambda (filename)
      ;; get-header takes an input port and returns a string of the 
      ;; character representations of the first 6 bytes of the file.  
      (define get-header
        (lambda (p)
          ;; byte-checker takes a byte.  If the byte is an integer
          ;; and is between 0 and 255 inclusive, the byte's 
          ;; character representation is returned.  Otherwise
          ;; #\t is returned.
          (define byte-checker 
            (lambda (byte)
              (if (and (integer? byte) (<= 0 byte 255))
                  (integer->char byte)
                  #\t)))
          (let* ([b0 (get-byte p)]
                 [b1 (get-byte p)]
                 [b2 (get-byte p)]
                 [b3 (get-byte p)]
                 [b4 (get-byte p)]
                 [b5 (get-byte p)])
            (apply string
                   (map byte-checker `(,b0 ,b1 ,b2 ,b3 ,b4 ,b5))))))
      (let* ([p (open-binary-input-file filename)] [header (get-header p)])
        (if (not (or (string=? header "GIF89a")
                     (string=? header "GIF87a")))
            (error 'read-image-gif "not a valid gif file"))
        (read-two-byte-number p) ; read and ignore number of screen columns
        (read-two-byte-number p) ; read and ignore number of screen rows
        (let* ([pack-byte (get-byte p)]
               [has-global-table? (not (zero? (logand pack-byte #b10000000)))]
               [table-num (logand pack-byte #b111)]
               [pixel-num (get-byte p)])
          (get-byte p) ; ignore background color
          (let  ([global-table 
                  (when has-global-table?
                    (read-color-table p 
                                      (shift-left 1 
                                                  (add1 table-num))))])
            (skip-loop p)
            (get-byte p) 
            (read-two-byte-number p) ; read and ignore start-left
            (read-two-byte-number p) ; read and ignore start-top
            (let* ([cols (read-two-byte-number p)]
                   [rows (read-two-byte-number p)]
                   [pack-image (get-byte p)]
                   [local-table? (if (zero? (logand pack-image #b10000000))
                                     (if (not has-global-table?)
                                         (error 'read-image 
                                                "The gif file is not complete -- cannot convert to an image")
                                         #f)
                                     #t)]
                   [interlaced? (not (zero? (logand pack-image #b1000000)))]
                   [local-table-num (logand pack-image #b111)]
                   [local-table (when local-table?
                                  (read-color-table p (shift-left 1 (add1 local-table-num))))]
                   [min-code-size (get-byte p)]
                   [clear-code (shift-left 1 min-code-size)] ;; 2^min-code-size
                   [EOI-code (add1 clear-code)]
                   [code-size (add1 min-code-size)]
                   [num-bytes (get-byte p)]
                   [table (if local-table?
                              local-table
                              global-table)]
                   [dictionary (make-vector MAX_DIC_SIZE (vector '(-1) -1))]
                   [curr-idx-ls '()]
                   [byte-i 8] 
                   [byte 0]
                   [counter 1]
                   [code 0] 
                   [old 0]
                   [index-vec (make-vector (* rows cols))]
                   [image (make-image rows cols)]
                   [data-vec (vector curr-idx-ls code-size 
                                     byte-i byte counter num-bytes code old)])
              ;; data vector key
              ;; variable    index
              ;; curr-idx-ls  0
              ;; code-size   1
              ;; byte-i      2
              ;; byte        3
              ;; counter     4
              ;; num-bytes   5
              ;; code        6
              ;; old         7
              (start-dic! dictionary table clear-code EOI-code)
              (let data-loop ([image-counter 0][dic-index EOI-code])
                (read-and-unpack! data-vec p)
                (cond
                 [(= (get-code data-vec) EOI-code)
                  (when interlaced?
                    (interlacer image rows cols index-vec table))
                  (close-input-port p)
                  image]
                 [(= (get-code data-vec) clear-code)
                  (reset-dic! dictionary (add1 EOI-code))
                  (vector-set! data-vec CODE_SIZE_IDX_R (add1 min-code-size))
                  (vector-set! data-vec CODE_IDX 0)
                  (data-loop image-counter EOI-code)]
                 [(= dic-index EOI-code)
                  (vector-set! data-vec IDX_LIST_IDX 
                               (list (get-code data-vec)))         
                  (image-handler! image index-vec table 
                                  cols dictionary (get-code data-vec)
                                  image-counter interlaced?)
                  (vector-set! data-vec OLD_IDX (get-code data-vec))
                  (vector-set! data-vec CODE_IDX 0)
                  (data-loop (add1 image-counter) (add1 dic-index))]
                 [(>= dic-index MAX_DIC_SIZE)
                  (image-handler! image index-vec table 
                                  cols dictionary (get-code data-vec) 
                                  image-counter interlaced?)
                  (vector-set! data-vec OLD_IDX (get-code data-vec)) 
                  (vector-set! data-vec CODE_IDX 0)
                  (data-loop 
                   (+ image-counter
                      (get-trans-length dictionary (get-old data-vec)))
                   dic-index)]
                 [(not(equal? '(-1) (lookup dictionary (get-code data-vec))))
                  (vector-set! data-vec IDX_LIST_IDX
                               (make-dictionary-entry dictionary 
                                                      (get-old data-vec)
                                                      (get-code data-vec)))
                  (add-to-dic! dictionary (get-dic-idx-list data-vec) 
                               dic-index (get-old data-vec))
                  (vector-set! data-vec CODE_SIZE_IDX_R
                               (compute-code-size (get-code-size data-vec) 
                                                  dic-index))
                  (image-handler! image index-vec table cols 
                                  dictionary (get-code data-vec) 
                                  image-counter interlaced?)
                  (vector-set! data-vec OLD_IDX (get-code data-vec)) 
                  (vector-set! data-vec CODE_IDX 0)
                  (data-loop 
                   (+ image-counter (get-trans-length dictionary 
                                                      (get-old data-vec)))
                   (add1 dic-index))]
                 [else
                  (let ([old-code (get-old data-vec)])
                    (vector-set! data-vec IDX_LIST_IDX
                                 (make-dictionary-entry dictionary 
                                                        old-code old-code))
                    (add-to-dic! dictionary (get-dic-idx-list data-vec)
                                 dic-index old-code)
                    (vector-set! data-vec CODE_SIZE_IDX_R
                                 (compute-code-size (get-code-size data-vec) 
                                                    dic-index))
                    (image-handler! image index-vec table cols 
                                    dictionary (get-code data-vec)
                                    image-counter interlaced?)
                    (vector-set! data-vec OLD_IDX (get-code data-vec))
                    (vector-set! data-vec CODE_IDX 0)
                    (data-loop  
                     (+ image-counter 
                        (get-trans-length dictionary (get-old data-vec)))
                     (add1 dic-index)))]))
              ))))))

  ;; start-dic! takes a color-table (a vector of colors), and two
  ;; codes, the clear code and the EOI code and initializes the
  ;; dictionary through mutation of a vector. The initialized
  ;; dictonary contains an entry for each entry in the color-table.
  ;; Those entries are vectors containing a list of one color table
  ;; index and its length.  The other two entries are vectors
  ;; containing a list of one of the two special codes (clear code or
  ;; EOI code) and its length. Both special codes have entries in the
  ;; dictionary.
  
  (define start-dic!
    (lambda (dic table clear-code EOI-code)
      (let ([size (vector-length table)])
        (let loop ([i 0])
          (when (< i size)
            (vector-set! dic i (vector (list i) 1))
            (loop (add1 i))))
        (vector-set! dic clear-code (vector (list clear-code) 1))
        (vector-set! dic EOI-code (vector (list EOI-code) 1))))) 

  ;; add-to-dic! takes a dictionary, a list of color table indices, a
  ;; dictionary index, and old, the previously examined code.  Mutates
  ;; the dictionary by adding an entry.  The entry is placed in the
  ;; next empty entry. The entry is a vector.  This vector contaims
  ;; the list of color table indices and the length of the list.  The
  ;; list's length is one more than the length of the tranlation, a
  ;; list of color table indices, of old.
  
  (define add-to-dic!
    (lambda (dic trans index old)
      (let ([length-trans (add1 (get-trans-length dic old))])
        (vector-set! dic index (vector trans length-trans)))))

  ;; reset-dic takes a dictionary and an index. Mutates the dictionary
  ;; by reseting all of the entries starting at the given index and
  ;; ending atthe last index of the dictionary.  When a dicitionary
  ;; entry is reset it is changed to (vector '(-1) -1).
  
  (define reset-dic!
    (lambda (dic index)
      (let ([size (vector-length dic)])
        (let loop ([i index])
          (when (< i size)
            (vector-set! dic i (vector '(-1) -1))
            (loop (add1 i)))))))

  ;; lookup takes a dictionary and a code. Returns the code's
  ;; translation. The translation is the element at index 0 in a
  ;; dictionary entry.  If a code is not in the dictionary, its
  ;; translation is given as '(-1).
  
  (define lookup
    (lambda (dic code)
      (vector-ref (vector-ref dic code) TRANS_IDX)))
  
  ;; get-trans-length takes a dictionary and a code. Returns the
  ;; length of the code's translation, the number of color table
  ;; indices in the translation. The length of the translation is the
  ;; element at index 1 in a dictionary entry.
  
  (define get-trans-length
    (lambda (dic code)
      (vector-ref (vector-ref dic code) LENGTH_IDX)))
  
  ;; compute-code-size takes a code size, the number of bits in a code,
  ;; and a dictionary index and returns the next code size.
  
  (define compute-code-size
    (lambda (code-size dic-idx)
      (if (and (= dic-idx (sub1(shift-left 1 code-size)))
               (< code-size 12))
          (add1 code-size)
          code-size)))
  
  ;; get-translation takes a data vector and returns the newest list
  ;; of color table indices.
  
  (define get-dic-idx-list
    (lambda (data-vec)
      (vector-ref data-vec IDX_LIST_IDX)))
  
  ;; get-code-size takes a data vector and returns the current code size.

  (define get-code-size
    (lambda (data-vec)
      (vector-ref data-vec CODE_SIZE_IDX_R)))
  
  ;; get-code takes a data vector and returns the current code.

  (define get-code
    (lambda (data-vec)
      (vector-ref data-vec CODE_IDX)))

  ;; get-old takes a date vector and returns the current old.
  
  (define get-old
    (lambda (data-vec)
      (vector-ref data-vec OLD_IDX)))
  
  ;; make-dictionary-entry
  ;; Takes two codes, code1 and code2, and a dictionary and returns 
  ;; a list of color table indices. The list is made by appending 
  ;; the first index of code2's translation to the end of code1's 
  ;; translation.
  
  (define make-dictionary-entry
    (lambda (dictionary code1 code2)
      (let* ([code1-entry (lookup dictionary code1)]
             [code2-entry (lookup dictionary code2)])
        (append code1-entry (list (car code2-entry))))))

  ;; image-maker! takes an image, the number of columns in the image,
  ;; a translation of a code, an image counter, and a color table.
  ;; The producture computes the starting row and column from the
  ;; image counter. The image is mutated by adding the pixcels between
  ;; the computed starting indices and the indices cooresponding to
  ;; the last pixcel represented in the code translation to the image.
  
  (define image-maker!
    (lambda (image cols ls counter table)
      (let* ([vec (list->vector ls)]
             [size (vector-length vec)]
             [start-row (quotient counter cols)]
             [start-col (modulo counter cols) ])
        (let loop ([row start-row] [col start-col] [i 0])
          (when (< i size)
            (cond 
             [(>= col cols)
              (loop (add1 row) 0 i)]
             [else
              (image-set! image row col (vector-ref table (vector-ref vec i)))
              (loop row (add1 col) (add1 i))]))))))
  
  ;; read-and-unpack! takes a data vector and an input port, reads 
  ;; bytes from a file, and unpacks the codes contained in the bytes.  
  ;; In a gif, the data byes are are in sub blocks, and the first byte 
  ;; in the block is the number of bytes of data in the block.  First,
  ;; all of the necessary information is obtained from the data vector.
  ;; Next a loop is entered. This loop is only exited when the code is 
  ;; built and the updated information is placed in the data vector.
  ;; building a code
  ;; If count (the number of bytes that have been read in a data 
  ;; sub-block) is more than the number of bytes in the sub-block, 
  ;; count is reset, and num-bytes is updated to be the next
  ;; byte in the file, the number of bytes in the next data 
  ;; sub-block.  If byte-i, the byte index, is 8, byte-i is zeroed, 
  ;; reset to the rigtmost bit in a byte, the next byte is read, 
  ;; and count is increased by 1. Otherwise, byte-i is increased by
  ;; 1, a bit is added to the code,and the code-i, the code index, is
  ;; increased by 1.
  
  (define read-and-unpack!
    (lambda (data-vec p)
      (let loop ([code-size (get-code-size data-vec)]
                 [byte-i (vector-ref data-vec BYTE_I_IDX_R)]
                 [byte (vector-ref data-vec BYTE_IDX_R)]
                 [counter (vector-ref data-vec COUNTER_IDX)]
                 [num-bytes (vector-ref data-vec NUM_BYTES_IDX)]
                 [code (get-code data-vec)]
                 [code-i 0])
        (cond
         [(> counter num-bytes)
          (loop code-size byte-i byte 1 (get-byte p) code code-i)]
         [(>= code-i code-size)
          (vector-set! data-vec BYTE_I_IDX_R byte-i)
          (vector-set! data-vec BYTE_IDX_R byte)
          (vector-set! data-vec COUNTER_IDX counter)
          (vector-set! data-vec NUM_BYTES_IDX num-bytes)
          (vector-set! data-vec CODE_IDX code)]
         [(>= byte-i 8)
          (loop code-size 0 (get-byte p) (add1 counter) num-bytes code code-i)]
         [else
          (loop code-size (add1 byte-i) byte counter num-bytes
                (+ (shift (logand byte (shift-left 1 byte-i)) 
                          (- code-i byte-i)) code)
                (add1 code-i))]))))
  
  ;; grow-image-vector! takes a vector of color table indices, and a
  ;; list of color indices.  Mutate the vector by adding the color
  ;; table indices in the list to the end of the vector.
  
  (define grow-index-vector!
    (lambda (idx-vec index ls)
      (let* ([vec (list->vector ls)]
             [size (vector-length vec)])
        (let loop ([i index] [vec-i 0])
          (when (< vec-i size)
            (vector-set! idx-vec i (vector-ref vec vec-i))
            (loop (add1 i)(add1 vec-i)))))))
  
  ;; image-handler! takes an image, a vector of color table indices, a
  ;; color table, the number of columns in the image, a dictionary, a
  ;; code, an image counter, and an interlace-flag. If the interlag is
  ;; true, calls grow-index-vector!. Otherwise calls image-maker!
  
  (define image-handler!
    (lambda (img idx-vec clr-table cols dic code img-counter interlace-flag)
      (let ([dic-entry (lookup dic code)])
        (if interlace-flag
            (grow-index-vector! idx-vec img-counter dic-entry)
            (image-maker! img cols dic-entry img-counter clr-table)))))

  ;; write-imag-gif takes an image and writes it to the given file in
  ;; the gif format.

  (define write-image-gif 
    (lambda (img filename)
      
      ;; image data vector key
      ;; variable     index
      ;; org-num-clr  0
      ;; clr-vec      1
      (let* ([p (open-binary-output-file filename)]
             [cols (image-cols img)]
             [rows (image-rows img)]
             [image-data-vec (count-colors 
                              (vector-sort color-compare (image->vector img)))]
             [org-num-clr (vector-ref image-data-vec NUM_CLRS_IDX)]
             [clr-vec (vector-ref image-data-vec CLR_VEC_IDX)]
             [many-colors? (> org-num-clr MAX_NUM_COLORS)])
        (when many-colors?
          (color-reducer! img))
        (let* ([clr-table-num
                (if many-colors?
                    MAX_TABLE_NUM
                    (inexact->exact (max 0 (sub1 (ceiling 
                                                  (logn org-num-clr 2))))))]
               [clr-table
                (if many-colors?
                    standard-clr-table
                    (make-color-table 
                     (shift-left 1 (add1 clr-table-num)) clr-vec))]
               [clr-octtree 
                (if many-colors?
                    standard-octtr
                    (table->octtree clr-table org-num-clr))])
          (write-header p)
          (write-two-byte-number cols p)
          (write-two-byte-number rows p)
          (put-byte (make-packed-byte 1 clr-table-num 0 clr-table-num) p)
          (put-byte 0 p)
          (put-byte 0 p)
          (write-clr-table clr-table p)
          (put-byte IMAGE_DATA_START p)
          (put-byte 0 p)
          (put-byte 0 p)
          (put-byte 0 p)
          (put-byte 0 p)
          (write-two-byte-number cols p)
          (write-two-byte-number rows p)
          (put-byte 0 p)
          (let* ([min-code-size (max 2 (add1 clr-table-num))]
                 [clear-code (shift-left 1 min-code-size)] ;;  2^min-code-size 
                 [EOI-code (add1 clear-code)]
                 [dic (empty-tree)]
                 [idx-buffer '()]
                 [curr-tbl-idx -1]
                 [buffer&curr-idx '()]
                 [code 0]
                 [byte-i 0]
                 [byte 0]
                 [output-i 0]
                 [code-size (add1 min-code-size)]
                 [info-vec (vector byte-i byte output-i code-size)]
                 [out-vec (make-vector 255 -1)]
                 [buffer-vec (vector curr-tbl-idx idx-buffer 
                                     dic buffer&curr-idx)])
            
            ;; buffer-vec key
            ;; variable         index
            ;; curr-tbl-idx     0
            ;; idx-buffer       1
            ;; dic              2 
            ;; buffer&curr-idx  3
            
            ;; info-vec key
            ;; variable    index
            ;; byte-i      0
            ;; byte        1
            ;; output-i    2 
            ;; code-size   3
            
            (pack-and-write! info-vec out-vec clear-code p EOI-code)
            (vector-set! buffer-vec DIC_IDX 
                         (initialize-dic clr-table clear-code EOI-code))
            (put-byte  min-code-size p) 
            (let loop ([row 0][col 0] [dic-idx (add1 EOI-code)])
              (when (< row rows)
                (cond
                 [(= col cols)
                  (loop (add1 row) 0 dic-idx)]
                 [else
                  (vector-set! buffer-vec TBL_IDX_IDX
                               (color-find-octtree clr-octtree 
                                                   (image-ref img row col)))
                  (vector-set! buffer-vec BUFFER&IDX_IDX
                               (cons (get-curr-table-idx buffer-vec)
                                     (get-index-buffer buffer-vec)))
                  (cond 
                   [(bst-dic-find (get-dic buffer-vec) 
                                  (get-buffer&curr-idx buffer-vec))
                    (vector-set! buffer-vec BUFFER_IDX
                                 (get-buffer&curr-idx buffer-vec))
                    (loop row (add1 col) dic-idx)]
                   [else
                    (let ([code-num (sub1 dic-idx)])
                      (vector-set! buffer-vec DIC_IDX 
                                   (add-to-dic-write 
                                    (get-dic buffer-vec) 
                                    (get-buffer&curr-idx buffer-vec) 
                                    dic-idx))
                      (set! code (bst-dic-find (get-dic buffer-vec) 
                                               (get-index-buffer buffer-vec)))
                      (pack-and-write! info-vec out-vec code 
                                       p EOI-code)
                      (cond
                       [(= code-num MAX_CODE)
                        (pack-and-write! info-vec out-vec clear-code 
                                         p EOI-code)
                        (vector-set! buffer-vec DIC_IDX 
                                     (initialize-dic clr-table 
                                                     clear-code EOI-code))
                        (vector-set! buffer-vec BUFFER_IDX '())
                        (vector-set! info-vec CODE_SIZE_IDX_W
                                     (add1 min-code-size))
                        (loop row col (add1 EOI-code))]
                       [else
                        (vector-set! buffer-vec BUFFER_IDX
                                     (list (get-curr-table-idx buffer-vec)))
                        (vector-set! info-vec CODE_SIZE_IDX_W 
                                     (compute-code-size 
                                      (get-code-size-write info-vec) code-num)) 
                        (loop row (add1 col) (add1 dic-idx))]))])])))
            (unless (null? (get-index-buffer buffer-vec))
              (pack-and-write! info-vec out-vec
                               (bst-dic-find (get-dic buffer-vec) 
                                             (get-index-buffer buffer-vec))
                               p EOI-code))
            (pack-and-write! info-vec out-vec EOI-code p EOI-code)
            (unless (zero? (vector-ref info-vec OUTPUT_I_IDX))
              (data-writer (vector-ref info-vec OUTPUT_I_IDX) out-vec p))
            (put-byte 0 p)
            (put-byte TRAILER p)
            (close-output-port p))))))
  
  ;; get-buffer&curr-idx takes a data vector, data-vec, and returns
  ;; the current buffer&curr-idx, a list that contains all of the
  ;; color table indices that are in the index buffer and the color
  ;; table index of current image element.
  
  (define get-buffer&curr-idx
    (lambda (data-vec)
      (vector-ref data-vec BUFFER&IDX_IDX)))
  
  ;; get-dic takes a data-vector and returns the current dictionary.
  
  (define get-dic
    (lambda (data-vec)
      (vector-ref data-vec DIC_IDX)))
  
  ;; get-code-size-write takes a data vector and returns the current
  ;; code-size.
  
  (define get-code-size-write
    (lambda (data-vec)
      (vector-ref data-vec CODE_SIZE_IDX_W)))
  
  ;; get-curr-table-idx takes a data vector, buffer-vec, and Returns
  ;; the the color table index of the current image element that is
  ;; being handled.
  
  (define get-curr-table-idx
    (lambda (buffer-vec)
      (vector-ref buffer-vec TBL_IDX_IDX)))
  
  ;; get-index-buffer takes a data vector, buffer-vec and returns the
  ;; current index buffer.
  
  (define get-index-buffer
    (lambda (buffer-vec)
      (vector-ref buffer-vec BUFFER_IDX)))
  
  ;; write-header writes the header of a gif file (GIF89a) to the 
  ;; given outputport.

  (define write-header
    (lambda (p)
      (put-byte 71 p)
      (put-byte 73 p)
      (put-byte 70 p)
      (put-byte 56 p)
      (put-byte 57 p)
      (put-byte 97 p)))

  ;; write-two-byte-number takes a number and writes it to the given 
  ;; output port as a two byte number.
  ;; (ex 10 => 10 0
  ;;    256 => 0 1)

  (define write-two-byte-number
    (lambda (num p)
      (put-byte (logand num 255) p)
      (put-byte (shift-right num 8) p)))

  ;; make-color-table takes a number representing the size of the
  ;; table and an a vector containg all of the different colors in the
  ;; image. If there are more colors in the vector than there are
  ;; entries in the table, the procedure terminates with an error
  ;; message. Returns a table containing all of the different colors
  ;; in an image. The table is a vector of vectors. Each vector
  ;; contains a color and index.
  
  (define make-color-table
    (lambda (size clr-vec)
      (let ([clr-table (make-empty-clr-table size)]
            [clr-vec-size (vector-length clr-vec)])
        (let loop ([i 0] )
          (when (< i clr-vec-size)
            (cond
             [(>= i size)
              (error 'write-image "too many colors -- cannot convert to gif")]
             [else (let ([clr (vector-ref clr-vec i)])
                     (vector-set! (vector-ref clr-table i) CLR_IDX clr)
                     (loop (add1 i) ))])))
        clr-table)))

  ;; write-clr-table takes a color table and writes it to the given
  ;; output port.
  
  (define write-clr-table
    (lambda (table p)
      (vector-for-each (lambda (vec) 
                         (write-color (vector-ref vec CLR_IDX) p)) 
                       table)))

  ;; make-packed-byte takes 4 numbers and returns a packed byte 
  ;; that contains those numbers.
  
  (define make-packed-byte
    (lambda (num1 num2 num3 num4)
      (+ (shift-left num1 7)
         (shift-left num2 4)
         (shift-left num3 3)
         num4)))

  ;; start-dic-write takes a color-table and a dictionary, an empty
  ;; tree.  Each entry in the dictionary is added to the dictionary
  ;; using add-to-dic-write. The entries are added in the following
  ;; way.  If count, a number between 0 and one less than the size of
  ;; the color table, is even, entries are added for color-table
  ;; entries at the start of the color table. Otherwise are added for
  ;; color-table entries at the end of the color table. Returns the
  ;; initialized dictionary. The initialized dictionary contains
  ;; entries for all of the color table entries. A dictionary entry is
  ;; a vector containg a list of color table indices and the
  ;; corresponing code. (There are two exceptions - the clear and EOI
  ;; codes)
  
  (define start-dic-write
    (lambda (clr-table dic)
      (let ([size (vector-length clr-table)])
        (let loop ([i 0] [j (sub1 size)] [count 0] [bst-dic dic])
          (cond
           [(>= count size) bst-dic]
           [(even? count)
            (loop (add1 i) j (add1 count) 
                  (add-to-dic-write bst-dic (list i) i))]
           [else (loop i (sub1 j) (add1 count) 
                       (add-to-dic-write bst-dic(list j) j))])))))

  ;; add-to-dic-write takes a dictionary, translation of a code and
  ;; code, and adds an entry to the dictionary. The dictionary with
  ;; the added entry is returned.  An entry consists of a translation,
  ;; and a code.
  
  (define add-to-dic-write

    ;; insert takes a binary relation rel?, an item x, and a binary
    ;; search tree ordered according to rel?. Returns a BST with the
    ;; same shape and data as the given BST but with one more leaf
    ;; containing x at the appropriate location.
    
    (letrec ([insert
              (lambda (rel? x bst)
                (cond
                 [(empty-tree? bst) (leaf x)]
                 [(rel? x (root-value bst)) 
                  (tree (root-value bst)
                        (insert rel? x (left-subtree bst))
                        (right-subtree bst))]
                 [else (tree (root-value bst)
                             (left-subtree bst)
                             (insert rel? x (right-subtree bst)))]))])
      
      (lambda (dic idx-list index)
        (insert (lambda (vec1 vec2)
                  (idx-list-compare (vector-ref vec1 TRANS_IDX)
                                    (vector-ref vec2 TRANS_IDX)))
                (vector idx-list index)
                dic))))

  ;; initialize-dic takes a color table, clr-table, the clear code,
  ;; and the EOI code, and returns an initialized dictionary.  An
  ;; initialized dictionary contains and entry for each entry of the
  ;; color table, an entry for the clear code, and an entry for the
  ;; EOI code.
  
  (define initialize-dic
    (lambda (clr-table clear-code EOI-code)
      (let* ([dic (empty-tree)]
             [started-dic (start-dic-write clr-table dic)])
        (add-to-dic-write 
         (add-to-dic-write started-dic (list clear-code) clear-code)
         (list EOI-code) EOI-code))))
  
  ;; idx-list-compare takes two lists of color table indices, idx-ls1
  ;; and idx-ls2 and returns true if and only if idx-ls1 converted to
  ;; a number is less than idx-ls2 converted to a number.
  
  (define idx-list-compare
    (lambda (idx-ls1 idx-ls2)
      (let ([num1 (list->number idx-ls1)]
            [num2 (list->number idx-ls2)])
        (< num1 num2))))
  
  ;; bst-dic-find takes a dictionary and a translation of a code and
  ;; looks for the tranlation in the dictionary.  If the translation
  ;; is in the dictionary, the corresponding code is returned.
  ;; Otherwise false is returned.

  (define bst-dic-find
    (lambda (dic idx-list)
      (cond
       [(empty-tree? dic) #f]
       [(equal? idx-list (vector-ref(root-value dic) TRANS_IDX))
        (vector-ref (root-value dic) DIC_CODE_IDX)]
       [(idx-list-compare idx-list 
                          (vector-ref (root-value dic) TRANS_IDX))
        (bst-dic-find (left-subtree dic) idx-list)]
       [else (bst-dic-find (right-subtree dic) idx-list)])))
  
  ;; color-compare takes two colors, clr1 and clr2 and returns true if
  ;; and if only if the numberic representation of clr1 is less than
  ;; the numberic representation of clr2.
  
  (define color-compare
    (lambda (clr1 clr2)
      (let ([red1 (color-ref clr1 'red)]
            [green1 (color-ref clr1 'green)]
            [blue1 (color-ref clr1 'blue)]
            [red2 (color-ref clr2 'red)]
            [green2 (color-ref clr2 'green)]
            [blue2 (color-ref clr2 'blue)])
        (< (+ (shift-left red1 16) (shift-left green1 8) blue1)
           (+ (shift-left red2 16) (shift-left green2 8) blue2)))))
  
  ;; list->number takes a list of numbers, converts the list to a
  ;; decimal number, and returns the decmial number.
  
  (define list->number
    (lambda (num-ls)
      (let helper ([ls num-ls] [num 0])
        (cond
         [(null? ls) num]
         [else (helper (cdr ls) (+ (* 10 num) (car ls)))]))))

  ;; make-empty-clr-table takes a number representing the size of a
  ;; table and returns an empty color-table. An empty color table is a
  ;; vector of vectors. Each vector contains the color black and an
  ;; index.
  
  (define make-empty-clr-table
    (lambda (size)
      (let ([vec (make-vector size)])
        (let loop ([i 0])
          (when (< i size)
            (vector-set! vec i (vector black i))
            (loop (add1 i))))
        vec)))
  
  ;; pack-and-write! takes a data vector, info-vec, a vector of bytes
  ;; which may contain -1's (empty entries), out-vec, a code, code, an
  ;; output port, p, and the EOI code.  If out-vec is full, 255 and
  ;; the contains of out-vec are written to the file. When a byte is
  ;; filled, it is added to out-vec. When all of the bits in the code
  ;; are packed into bytes, information in the data vector is updated.
  
  (define pack-and-write!
    (lambda (info-vec out-vec code p EOI-code)
      (let loop ([byte-i (vector-ref info-vec BYTE_I_IDX_W)]
                 [byte (vector-ref info-vec  BYTE_IDX_W)]
                 [output-i (vector-ref info-vec OUTPUT_I_IDX)]
                 [code-i 0]
                 [code-size (get-code-size-write info-vec)])
        (cond
         [(>= output-i 255)
          (put-byte output-i p)
          (vector-for-each (lambda (ele) (put-byte ele p))  out-vec)
          (loop byte-i byte 0 code-i code-size)]
         [(>= code-i code-size)
          (cond 
           [(and (= code EOI-code) (not (zero? byte-i)))
            (vector-set! out-vec output-i byte)
            (vector-set! info-vec BYTE_I_IDX_W byte-i)
            (vector-set! info-vec BYTE_IDX_W byte)
            (vector-set! info-vec OUTPUT_I_IDX (add1 output-i))]
           [else 
            (vector-set! info-vec BYTE_I_IDX_W byte-i)
            (vector-set! info-vec BYTE_IDX_W byte)
            (vector-set! info-vec OUTPUT_I_IDX output-i)])]
         [(>= byte-i 8)
          (vector-set! out-vec output-i byte)
          (loop 0 0 (add1 output-i) code-i code-size)]
         [else
          (loop (add1 byte-i)
                (+ (shift (logand code (shift-left 1 code-i))
                          (- byte-i code-i)) byte)
                output-i (add1 code-i) code-size)]))))
  
  ;; data-writer takes a number, num-data-bytes, that represents the
  ;; number of bytes to be written to the output port, a vector of
  ;; bytes, vec, and an output port, p.  Writes the bytes in the
  ;; vector between byte 0 and the byte whose index is one less than
  ;; num-data-bytes. Also, writes num-data-bytes to the output port.
  
  (define data-writer
    (lambda (num-data-bytes vec p)
      (let ([size (vector-length vec)])
        (put-byte num-data-bytes p)
        (let loop ([i 0])
          (when (and (< i size) (< i num-data-bytes))
            (put-byte (vector-ref vec i) p)
            (loop (add1 i))))))) 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; GIF helper procedures ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; image->vector takes an image, img, and returns a vector containing
  ;; all of the
  ;; entries in the image.
  
  (define image->vector
    (lambda (img)
      (let* ([rows (image-rows img)] [cols (image-cols img)]
             [size (* rows cols)] [vec (make-vector size)])
        (let loop ([i 0] [r 0] [c 0])
          (when (< i size)
            (cond
             [(>= c cols) (loop i (add1 r) 0)]
             [else (vector-set! vec i (image-ref img r c))
                   (loop (add1 i) r (add1 c))])))
        vec)))

  ;; skip-loop skips over all of the extensions in a single image 
  ;; gif file.
  
  (define skip-loop
    ;; first define some helpers
    (let ([skip-graphics
           ;; Skips over a graphics extension.
           (lambda (p)
             (file-position p (+ (file-position p) 7)))]
          [skip-text
           ;; Skips over all non-graphic extensions.
           (lambda (p)
             (let loop ([counter 1]
                        [num-bytes (get-byte p)])
               (unless (zero? num-bytes)
                 (cond
                  [(= counter (add1 num-bytes))
                   (loop 1 (get-byte p))]
                  [else (get-byte p)
                        (loop (add1 counter) num-bytes)]))))])
      (lambda (p)
        (when (= (peek-byte p) EXT_MARKER)
          (get-byte p)
          (cond
           [(= (peek-byte p) GRAPHICS_EXT_MARKER)
            (skip-graphics p)
            (skip-loop p)]
           [else
            (get-byte p)
            (skip-text p)
            (skip-loop p)])))))
  
  ;; count-colors takes a sorted vector of the colors of all of the
  ;; pixels in an image and returns a vector containing the number of
  ;; colors in the image and a vector. The vector contains all of the
  ;; different colors in the image.
  
  (define count-colors
    (lambda (sorted-clr-vec)
      (let ([size (vector-length sorted-clr-vec)])
        (let loop ([accum 1] [i 1]
                   [comp-clr (vector-ref sorted-clr-vec 0)]
                   [clr-ls '()])
          (cond
           [(>= i size)
            (let ([clr-vec (list->vector clr-ls)])
              (if (< (vector-length clr-vec) accum)
                  (vector accum (list->vector (cons comp-clr clr-ls)))
                  (vector accum clr-vec)))]
           [(color-equal? (vector-ref sorted-clr-vec i) comp-clr)
            (loop accum (add1 i) comp-clr clr-ls)]
           [else (loop (add1 accum) (add1 i) (vector-ref sorted-clr-vec i)
                       (cons comp-clr clr-ls))])))))
  
  ;; interlacer takes an image, the numer of rows and columns in the
  ;; image, a list of color table indices, and a color table.  This
  ;; procedure is used to help convert an interlaced gif in to an
  ;; image (returns an image).
  
  (define interlacer
    (lambda (img rows cols idx-vec table)
      (let ([i 0])
        (let ([help (lambda (init-r step)
                      (let rloop ([r init-r])
                        (when (< r rows)
                          (let cloop ([c 0])
                            (when (< c cols)
                              (image-set! img r c (vector-ref table (vector-ref idx-vec i)))
                              (set! i (add1 i))
                              (cloop (add1 c))))
                          (rloop (+ step r)))))])
          (help 0 8)
          (help 4 8)
          (help 2 4)
          (help 1 2)
          img))))
  
  ;; color-reducer! takes an image.  It replaces all of the colors in
  ;; the in the image with closest color in an octtree made from the
  ;; standard color table
  
  (define color-reducer!
    (lambda (img)
      (let ([clr-octree standard-octtr]
            [cols (image-cols img)]
            [rows (image-rows img)])
        (let loop ([r 0] [c 0])
          (when (< r rows)
            (cond
             [(>= c cols)
              (loop (add1 r) 0)]
             [else
              (image-set! img r c (find-octtree-match 
                                   clr-octree (image-ref img r c)))
              (loop r (add1 c))]))))))
  
  ;; standard-clr-table
  ;; This table is the table that is used to make the octtree for color
  ;; quantization in images that have more than 256 colors. 
  
  (define standard-clr-table
    (vector
     (vector (color 0 0 0) 0) (vector (color 17 0 0) 1)
     (vector (color 34 0 0) 2) (vector (color 68 0 0) 3)
     (vector (color 85 0 0) 4) (vector (color 119 0 0) 5)
     (vector (color 136 0 0) 6) (vector (color 170 0 0) 7) 
     (vector (color 187 0 0) 8) (vector (color 221 0 0) 9) 
     (vector (color 0 0 51) 10) (vector (color 0 0 102) 11)
     (vector (color 0 0 153) 12) (vector (color 0 0 204) 13) 
     (vector (color 238 0 0) 14) (vector (color 0 51 51) 15) 
     (vector (color 0 51 102) 16) (vector (color 0 51 153) 17) 
     (vector (color 0 51 204) 18) (vector (color 0 51 255) 19) 
     (vector (color 0 102 0) 20) (vector (color 0 102 51) 21) 
     (vector (color 0 102 102) 22) (vector (color 0 102 153) 23) 
     (vector (color 0 102 204) 24) (vector (color 0 102 255) 25) 
     (vector (color 0 153 0) 26) (vector (color 0 153 51) 27)
     (vector (color 0 153 102) 28) (vector (color 0 153 153) 29)
     (vector (color 0 153 204) 30) (vector (color 0 153 255) 31)
     (vector (color 0 204 0) 32) (vector (color 0 204 51) 33)
     (vector (color 0 204 102) 34) (vector (color 0 204 153) 35)
     (vector (color 0 204 204) 36) (vector (color 0 204 255) 37)
     (vector (color 0 255 51) 38) (vector (color 0 255 102) 39)
     (vector (color 0 255 153) 40) (vector (color 0 255 204) 41)
     (vector (color 0 17 0) 42) (vector (color 51 0 51) 43)
     (vector (color 51 0 102) 44) (vector (color 51 0 153) 45)
     (vector (color 51 0 204) 46) (vector (color 51 0 255) 47)
     (vector (color 0 34 0) 48) (vector (color 51 51 51) 49)
     (vector (color 51 51 102) 50) (vector (color 51 51 153) 51)
     (vector (color 51 51 204) 52) (vector (color 51 51 255) 53)
     (vector (color 51 102 0) 54) (vector (color 51 102 51) 55)
     (vector (color 51 102 102) 56) (vector (color 51 102 153) 57) 
     (vector (color 51 102 204) 58) (vector (color 51 102 255) 59)
     (vector (color 51 153 0) 60) (vector (color 51 153 51) 61)
     (vector (color 51 153 102) 62) (vector (color 51 153 153) 63)
     (vector (color 51 153 204) 64) (vector (color 51 153 255) 65)
     (vector (color 51 204 0) 66) (vector (color 51 204 51) 67)
     (vector (color 51 204 102) 68) (vector (color 51 204 153) 69)
     (vector (color 51 204 204) 70) (vector (color 51 204 255) 71)
     (vector (color 51 255 0) 72) (vector (color 51 255 51) 73)
     (vector (color 51 255 102) 74) (vector (color 51 255 153) 75)
     (vector (color 51 255 204) 76) (vector (color 51 255 255) 77)
     (vector (color 102 0 0) 78) (vector (color 102 0 51) 79)
     (vector (color 102 0 102) 80) (vector (color 102 0 153) 81)
     (vector (color 102 0 204) 82) (vector (color 102 0 255) 83)
     (vector (color 0 68 0) 84) (vector (color 102 51 51) 85)
     (vector (color 102 51 102) 86) (vector (color 102 51 153) 87)
     (vector (color 102 51 204) 88) (vector (color 102 51 255) 89)
     (vector (color 102 102 0) 90) (vector (color 102 102 51) 91)
     (vector (color 102 102 102) 92) (vector (color 102 102 153) 93)
     (vector (color 102 102 204) 94) (vector (color 102 102 255) 95)
     (vector (color 102 153 0) 96) (vector (color 102 153 51) 97)
     (vector (color 102 153 102) 98) (vector (color 102 153 153) 99)
     (vector (color 102 153 204) 100) (vector (color 102 153 255) 101)
     (vector (color 102 204 0) 102) (vector (color 102 204 51) 103)
     (vector (color 102 204 102) 104) (vector (color 102 204 153) 105)
     (vector (color 102 204 204) 106)(vector (color 102 204 255) 107)
     (vector (color 102 255 0) 108) (vector (color 102 255 51) 109)
     (vector (color 102 255 102) 110) (vector (color 102 255 153) 111)
     (vector (color 102 255 204) 112) (vector (color 102 255 255) 113)
     (vector (color 0 85 0) 114) (vector (color 153 0 51) 115)
     (vector (color 153 0 102) 116) (vector (color 153 0 153) 117)
     (vector (color 153 0 204) 118) (vector (color 153 0 255) 119)
     (vector (color 0 119 0) 120) (vector (color 153 51 51) 121)
     (vector (color 153 51 102) 122) (vector (color 153 51 153) 123)
     (vector (color 153 51 204) 124) (vector (color 153 51 255) 125)
     (vector (color 153 102 0) 126) (vector (color 153 102 51) 127)
     (vector (color 153 102 102) 128) (vector (color 153 102 153) 129)
     (vector (color 153 102 204) 130) (vector (color 153 102 255) 131)
     (vector (color 153	153 0) 132) (vector (color 153 153 51) 133)
     (vector (color 153 153 102) 134) (vector (color 153 153 153) 135)
     (vector (color 153 153 204) 136) (vector (color 153 153 255) 137)
     (vector (color 153 204 0) 138) (vector (color 153 204 51) 139)
     (vector (color 153 204 102) 140) (vector (color 153  204 153) 141)
     (vector (color 153 204 204) 142) (vector (color 153 204 255) 143)
     (vector (color 153 255 0) 144) (vector (color 153 255 51) 145)
     (vector (color 153 255 102) 146) (vector (color 153 255 153) 147)
     (vector (color 153 255 204) 148) (vector (color 153 255 255) 149)
     (vector (color 0 136 0) 150) (vector (color 204 0 51) 151)
     (vector (color 204 0 102) 152) (vector (color 204 0 153) 153)
     (vector (color 204 0 204) 154) (vector (color 204 0 255) 155)
     (vector (color 0 170 0) 156) (vector (color 204 51 51) 157)
     (vector (color 204 51 102) 158) (vector (color 204 51 153) 159)
     (vector (color 204 51 204) 160) (vector (color 204 51 255) 161)
     (vector (color 204 102 0) 162) (vector (color 204 102 51) 163)
     (vector (color 204 102 102) 164) (vector (color 204 102 153) 165)
     (vector (color 204 102 204) 166) (vector (color 204 102 255) 167)
     (vector (color 204 153 0) 168) (vector (color 204 153 51) 169)
     (vector (color 204 153 102) 170) (vector (color 204 153 153) 171)
     (vector (color 204 153 204) 172) (vector (color 204 153 255) 173)
     (vector (color 204 204 0) 174) (vector (color 204 204 51) 175)
     (vector (color 204 204 102) 176) (vector (color 204 204 153) 177)
     (vector (color 204 204 204) 178) (vector (color 204 204 255) 179)
     (vector (color 204 255 0) 180) (vector (color 204 255 51) 181)
     (vector (color 204 255 102) 182) (vector (color 204 255 153) 183)
     (vector (color 204 255 204) 184) (vector (color 204 255 255) 185)
     (vector (color 255 0 51) 186) (vector (color 255 0 102) 187)
     (vector (color 255 0 153) 188) (vector (color 255 0 204) 189)
     (vector (color 255 51 0) 190) (vector (color 255 51 51) 191)
     (vector (color 255 51 102) 192) (vector (color 255 51 153) 193)
     (vector (color 255 51 204) 194) (vector (color 255 51 255) 195)
     (vector (color 255 102 0) 196) (vector (color 255 102 51) 197)
     (vector (color 255 102 102) 198) (vector (color 255 102 153) 199)
     (vector (color 255 102 204) 200) (vector (color 255 102 255) 201)
     (vector (color 255 153 0) 202) (vector (color 255 153 51) 203)
     (vector (color 255 153 102) 204) (vector (color 255 153 153) 205)
     (vector (color 255 153 204) 206) (vector (color 255 153 255) 207)
     (vector (color 255 204 0) 208) (vector (color 255 204 51) 209)
     (vector (color 255 204 102) 210) (vector (color 255 204 153) 211)
     (vector (color 255 204 204) 212) (vector (color 255 204 255) 213)
     (vector (color 255 255 51) 214) (vector (color 255 255 102) 215)
     (vector (color 255 255 153) 216) (vector (color 255 255 204) 217)
     (vector (color 0 187 0) 218) (vector (color 0 221 0) 219)
     (vector (color 0 238 0) 220) (vector (color 0 0 17) 221)
     (vector (color 0 0 34) 222) (vector (color 0 0 68) 223)
     (vector (color 0 0 85) 224) (vector (color 0 0 119) 225)
     (vector (color 0 0 136) 226) (vector (color 0 0 170) 227)
     (vector (color 0 0 187) 228) (vector (color 0 0 221) 229)
     (vector (color 0 0 238) 230) (vector (color 17 17 17) 231)
     (vector (color 34 34 34) 232) (vector (color 68 68 68) 233)
     (vector (color 85 85 85) 234) (vector (color 119 119 119) 235)
     (vector (color 136 136 136) 236) (vector (color 51 0 0) 237) 
     (vector (color 238 238 238) 238) (vector (color 153 0 0) 239)
     (vector (color 204 0 0) 240) (vector (color 0 51 0) 241)
     (vector (color 51 51 0) 242) (vector (color 102 51 0) 243)
     (vector (color 153 51 0) 244) (vector (color 204 51 0) 245)
     (vector (color 170 170 170) 246) (vector (color 187 187 187) 247)
     (vector (color 221 221 221) 248) (vector (color 255 0 0) 249) 
     (vector (color 0 255 0) 250) (vector (color 255 255 0) 251)
     (vector (color 0 0 255) 252) (vector (color 255 0 255) 253)
     (vector (color 0 255 255) 254) (vector (color 255 255 255) 255)))

  ;; standard-octtr is the octtree used for color quantization
  ;; in images that have more than 256 colors.
  
  (define standard-octtr (table->octtree standard-clr-table 255))
  
  (set! read-gif-image read-image-gif)
  (set! write-gif-image write-image-gif)
  )

|#