#lang racket

(provide
 (contract-out
  [draw-tree      (tree/c . -> . void?)]
  [empty-tree     (-> tree/c)]
  [empty-tree?    (any/c . -> . boolean?)]
  [leaf           (any/c . -> . tree/c)]
  [leaf?          (any/c . -> . boolean?)]
  [left-subtree   (tree/c . -> . tree/c)]
  [left-subtree?  (tree/c . -> . boolean?)]
  [right-subtree  (tree/c . -> . tree/c)]
  [right-subtree? (tree/c . -> . boolean?)]
  [root-value     (tree/c . -> . any/c)]
  [tree           (any/c tree/c tree/c . -> . tree/c)]
  [tree?          (any/c . -> . boolean?)]))

; To draw trees
(require slideshow/pict
         slideshow/pict-convert)

; Structures for trees
(define-struct empty-tree () #:transparent
  #:methods gen:custom-write
  [(define (write-proc tr port mode)
     (fprintf port "(empty-tree)"))])
  
(define-struct tree (value left right)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc tr port mode)
     (if (or (left-subtree? tr) (right-subtree? tr))
         (fprintf port "(tree ~s ~s ~s)" (root-value tr) (left-subtree tr) (right-subtree tr))
         (fprintf port "(leaf ~s)" (root-value tr))))]
  #:property prop:pict-convertible 
  (λ (tr) (draw-tree tr)))

; Union contract so that trees can be empty or not
(define tree/c (or/c empty-tree? tree?))

; Create or test if something is a leaf (tree with no children)
(define (leaf value) (tree value (empty-tree) (empty-tree)))
(define (leaf? tr) 
  (and (tree? tr)
       (empty-tree? (tree-left tr))
       (empty-tree? (tree-right tr))))

; Rename left and right subtree
; A straight define or rename-out doesn't fix the contract error message
(define (left-subtree tr) (tree-left tr))
(define (right-subtree tr) (tree-right tr))

; Check if the left/right subtree is non-empty
(define (left-subtree? tr) (not (empty-tree? (left-subtree tr))))
(define (right-subtree? tr) (not (empty-tree? (right-subtree tr))))

; Get the root value of a tree
(define (root-value tr) (tree-value tr))

; Draw a tree using slideshow/pict
(define (draw-tree tr)
  (match tr
    [(tree value left right)
     (define v (text (format "~a" value)))
     (define l (draw-tree left))
     (define r (draw-tree right))
     (pin-line (pin-line (vc-append 10 v (ht-append l r))
                         v cb-find
                         l ct-find
                         #:color (if (empty-tree? left) "white" "black"))
               v cb-find
               r ct-find
               #:color (if (empty-tree? right) "white" "black"))]
     [else
     (colorize (rectangle 12 12) "white")]))
