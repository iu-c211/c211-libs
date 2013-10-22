#lang racket

(provide
 (contract-out
  [draw-tree     (tree/c . -> . void?)]
  [empty-tree    (-> tree/c)]
  [empty-tree?   (any/c . -> . boolean?)]
  [leaf          (any/c . -> . tree/c)]
  [leaf?         (any/c . -> . boolean?)]
  [left-subtree  (tree/c . -> . tree/c)]
  [right-subtree (tree/c . -> . tree/c)]))
  
  

(require slideshow/pict)

(define-struct empty-tree ())
(define-struct tree (value left right))

(define tree/c (or/c empty-tree? tree?))

(define (leaf value) (tree value (empty-tree) (empty-tree)))

(define (leaf? tr) 
  (and (tree? tr)
       (empty-tree? (tree-left tr))
       (empty-tree? (tree-right tr))))

(define left-subtree tree-left)
(define right-subtree tree-right)

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
     (colorize (rectangle 40 20) "white")]))

(draw-tree
 (tree 8
       (tree 6 (leaf 7) (leaf 5))
       (tree 3 
             (empty-tree)
             (tree 0 (leaf 9) (empty-tree)))))