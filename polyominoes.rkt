(require "Graphic.rkt")

(define-struct pos (x y))

(define-struct state (puzzle pieces))

(define (solve-puzzle grid polys viz-style)
  (local
    [(define result
       (search 
        (lambda (S) (empty? (state-pieces S)))
        neighbours
        (cond
          [(symbol=? viz-style 'interactive)
           (lambda (S) (draw-grid (state-puzzle S)))]
          [else false])
        (make-state grid polys)))
     
     (define maybe-last-draw
       (cond
         [(and (state? result)
               (symbol=? viz-style 'at-end))
          (draw-grid (state-puzzle result))]
         [else false]))]
    (cond
      [(boolean? result) result]
      [else (map list->string (state-puzzle result))])))
      
      
;;(build-2dlist x y f) produces a list of lists containing the results of the
;;       consumed function(f) applied to all (x,y) positions corresponding to
;;       positions in the grid (bounded with a width of x and a height of y),
;;       in specific order.
;; build-2dlist: Nat Nat (Nat Nat -> X) -> (listof (listof X))
;; requires: Both x and y are non-zero

(define (build-2dlist x y f)
  (map (lambda (y) (build-list x (lambda (x) (f x y))))
       (build-list y (lambda (y) (+ 0 y)))))


;;(all-positions w h) produces a list of Pos containing all possible positions
;;       in a grid with width w and height h in a specific order.
;; all-positions: Nat Nat -> (listof Pos)
;; requires: Both w and h are non-zero

(define (all-positions w h)
  (foldr append empty (build-2dlist w h (lambda (x y) (make-pos x y)))))


;; (all-orientations my-grid) produces a list of Grids containing all distinct
;;       rotations and reflections of that polyomino in my-grid.
;; all-orientations: Grid -> (listof Grid)

(define (all-orientations my-grid)
  (local [;; (right-ori my-grid) produces a grid which is oriented 90 degree to
          ;;         the right from the consumed grid (my-grid).
          ;; right-ori: Grid -> Grid
          (define (right-ori my-grid)
            (cond [(empty? (first my-grid)) empty]
                  [else
                   (cons (reverse (map (lambda (x) (first x)) my-grid))
                         (right-ori (map (lambda (x) (rest x)) my-grid)))]))
          ;; (right-ori-rec i my-grid) produces a grid which is oriented
          ;;         90 degree to the right from the consumed grid (my-grid)
          ;;         with i number of times.
          ;; right-ori-rec: Nat Grid -> Grid
          (define (right-ori-rec i my-grid)
            (cond [(zero? i) (right-ori my-grid)]
                  [else (right-ori-rec (sub1 i) (right-ori my-grid))]))]
    (foldr (lambda (x y)
             (cons x (filter (lambda (z) (not (equal? x z))) y)))
           empty
           (append (build-list 4 (lambda (x) (right-ori-rec x my-grid)))
                   (build-list 4 (lambda (x)
                                   (right-ori-rec x(map (lambda (x)
                                                          (reverse x))
                                                        my-grid))))))))
                 
                 
;; (first-empty-pos my-grid) produces the Pos of the first #\. character in
;;       the my-grid consumed.If there is no #\. in my-grid, it produces false.
;; first-empty-pos: Grid -> (anyof Pos false)

(define (first-empty-pos my-grid)
  (local [;; (first-empty-pos/acc my-grid x-acc y-acc) produces the Pos of the
          ;;       first #\. character with in the my-grid consumed. If there
          ;;       is no #\. in my-grid, it produces false.
          ;; first-empty-pos/acc: Grid Nat Nat -> (anyof Pos false)
          (define (first-empty-pos/acc my-grid x-acc y-acc)
            (local [;; (first-empty-pos-row/acc my-row x-acc) produces the
                    ;;       position count of the first #\. character with in
                    ;;       the list of characters (my-row) consumed. If there
                    ;;       is no #\. in my-grid, it produces false.
                    ;; first-empty-pos-row/acc:
                    ;; (listof Char) Nat Nat -> (anyof Nat false)
                    (define (first-empty-pos-row/acc my-row x-acc)
                      (cond [(empty? my-row) false]
                            [(char=? (first my-row) #\.) x-acc]
                            [else (first-empty-pos-row/acc (rest my-row)
                                                           (add1 x-acc))]))]
              (cond [(empty? my-grid) false]
                    [(number? (first-empty-pos-row/acc (first my-grid) x-acc))
                     (make-pos (first-empty-pos-row/acc (first my-grid) x-acc)
                               y-acc)]
                    [else (first-empty-pos/acc (rest my-grid)
                                               x-acc (add1 y-acc))])))]
    (first-empty-pos/acc my-grid 0 0)))


;; (superimpose base top my-pos) produces a Grid in which top Grid(top) covers
;;       on top of the base Grid(base) with the consumed Pos(my-pos) indicating
;;       the location of the upper left corner of top, and the #\. of the top
;;       Grid does not overwrite the corresponing character of the base Grid.
;; superimpose: Grid Grid Pos -> Grid

(define (superimpose base top my-pos)
  (local [;; (row-laid/acc base-row top-row my-pos-x x-acc) producces a list of
          ;;         characters where top-row is overwriting the base-row with
          ;;         the corresponding character when x-acc is bigger or equal
          ;;         to my-pos-x except that the corresponding character in
          ;;         top-row is #\.
          ;; row-laid/acc: (listof Char) (listof Char) Nat Nat -> (listof Char)
          (define (row-laid/acc base-row top-row my-pos-x x-acc)
            (cond [(empty? base-row) empty]
                  [(empty? top-row)
                   (cons (first base-row)
                         (row-laid/acc (rest base-row)
                                       top-row my-pos-x (add1 x-acc)))]
                  [(< x-acc my-pos-x)
                   (cons (first base-row)
                         (row-laid/acc (rest base-row) top-row 
                                       my-pos-x (add1 x-acc)))]
                  [(char=? #\. (first top-row))
                   (cons (first base-row)
                         (row-laid/acc (rest base-row) (rest top-row) 
                                       my-pos-x (add1 x-acc)))]
                  [else (cons (first top-row)
                              (row-laid/acc (rest base-row) (rest top-row) 
                                            my-pos-x (add1 x-acc)))]))
                                            
                                            
;; (superimpose/acc base top my-pos x-acc y-acc) produces a Grid in
;;         which top Grid(top) covers on top of the base Grid(base)
;;         with the consumed Pos(my-pos) indicating the location of
;;         the upper left corner of top (which means the overwriting
;;         begins when x-acc is bigger or equal to x coordinates of
;;         my-pos and y-acc is bigger or equal to y coordinates of
;;         my-pos) , and the #\. of the top Grid does not overwrite
;;         the corresponing character of the base Grid.
;; superimpose/acc: Grid Grid Pos Nat Nat -> Grid
          (define (superimpose/acc base top my-pos x-acc y-acc)
            (cond [(empty? base) empty]
                  [(empty? top)
                   (cons (first base)
                         (superimpose/acc (rest base) top
                                          my-pos x-acc (add1 y-acc)))]            
                  [(< y-acc (pos-y my-pos))
                   (cons (first base)
                         (superimpose/acc (rest base)
                                          top my-pos x-acc (add1 y-acc)))]
                  [else (cons (row-laid/acc (first base) (first top)
                                            (pos-x my-pos) x-acc)
                              (superimpose/acc (rest base)
                                               (rest top) my-pos
                                               x-acc (add1 y-acc)))]))]
    (superimpose/acc base top my-pos 0 0)))


;; (legal? pzzl polyn insert-pt) produces true if the consumed polyomino (polyn)
;;       can be superimposed on top of the consumed puzzle (pzzl) at the
;;       consumed Pos (insert-pt) where each square of polyn lies in an
;;       in-bounds, empty position in pzzl, and one of those newly occupied
;;       positions is the first empty position in pzzl. Otherwise, it produces
;;       false.
;; legal?: Grid Grid Pos -> Bool

(define (legal? pzzl polyn insert-pt)
  (local [(define first-empty-pt (first-empty-pos pzzl))
          ;; (row-nth-item lst n) produces the n-th element in lst.
          ;; row-nth-item: (listof Any) Nat -> Any
          ;; requires: lst must be non-empty list
          (define (row-nth-item lst n)
            (cond [(empty? lst) empty]
                  [(< 0 n)(row-nth-item (rest lst) (sub1 n))]
                  [else (first lst)]))
          ;; (row-legal?/acc pzzl-row polyn-row insert-pt-x x/acc) produces a
          ;;       list of characters or false. If the polyn-row is in-bound of
          ;;       the pzzl-row and filling the empty spots in pzzl-row with no
          ;;       overlapping starting from insert-pt-x, it will produce the
          ;;       filled list of characters, otherwise, it will produe false
          ;;       at the corresponing element.
          ;; row-legal?/acc: (listof Char) (listof Char) Nat Nat
          ;;                 -> (listof (anyof Char false))
          (define (row-legal?/acc pzzl-row polyn-row insert-pt-x x/acc)
            (cond [(and (empty? pzzl-row)(empty? polyn-row)) empty]
                  [(empty? pzzl-row) (list false)]
                  [(empty? polyn-row)
                   (cons (first pzzl-row)
                         (row-legal?/acc (rest pzzl-row)
                                         polyn-row insert-pt-x (add1 x/acc)))]
                  
                  [(< x/acc insert-pt-x)
                   (cons (first pzzl-row)
                         (row-legal?/acc (rest pzzl-row) polyn-row 
                                         insert-pt-x (add1 x/acc)))]
                  [(char=? #\. (first polyn-row))
                   (cons (first pzzl-row)
                         (row-legal?/acc (rest pzzl-row) (rest polyn-row) 
                                         insert-pt-x (add1 x/acc)))]
                  [(char=? #\. (first pzzl-row))
                   (cons (first polyn-row)
                         (row-legal?/acc (rest pzzl-row) (rest polyn-row) 
                                         insert-pt-x (add1 x/acc)))]
                  [else (cons false
                              (row-legal?/acc (rest pzzl-row) (rest polyn-row) 
                                              insert-pt-x (add1 x/acc)))]))
                                              
;; (legal?/acc pzzl polyn insert-pt x/acc y/acc) produces true if the
;;       consumed polyomino (polyn) can be superimposed on top of the
;;       consumed puzzle (pzzl) at the consumed Pos (insert-pt) where
;;       each square of polyn lies in an in-bounds, empty position in
;;       pzzl, and one of those newly occupied positions is the first
;;       empty position in pzzl. Otherwise, it produces false.
;; legal?/acc: Grid Grid Pos Nat Nat -> Bool
          (define (legal?/acc pzzl polyn insert-pt x/acc y/acc)
            (cond [(empty? polyn) true]
                  [(empty? pzzl) false]
                  [(false? first-empty-pt) false]
                  [(or (< (pos-y first-empty-pt)(pos-y insert-pt))
                       (< (pos-x first-empty-pt)(pos-x insert-pt)))false]
                  [(< y/acc (pos-y insert-pt))
                   (legal?/acc (rest pzzl) polyn
                               insert-pt x/acc (add1 y/acc))]
                  [(= y/acc (pos-y first-empty-pt))
                   (cond [(and (not (member? false (row-legal?/acc
                                                    (first pzzl) (first polyn)
                                                    (pos-x insert-pt) x/acc)))
                               (not (char=? #\.(row-nth-item
                                                (row-legal?/acc
                                                 (first pzzl)
                                                 (first polyn)
                                                 (pos-x insert-pt) x/acc)
                                                (pos-x first-empty-pt)))))
                          (legal?/acc (rest pzzl) (rest polyn)
                                      insert-pt x/acc (add1 y/acc))]
                         [else false])]
                  [(member? false (row-legal?/acc
                                   (first pzzl)(first polyn)
                                   (pos-x insert-pt) x/acc)) false]
                  [else (legal?/acc (rest pzzl) (rest polyn)
                                    insert-pt x/acc (add1 y/acc))]))]
    (legal?/acc pzzl polyn insert-pt 0 0)))


;; (alorien-legal-offset pzzl lopolyn lolopos) produces a list of Grid which
;;       are all the possible combinations of pzzl being legally superimposed
;;       by every polyomino in lopolyn.
;; alori-legal-offset: Grid (listof Grid) (listof (listof Pos)) -> (listof Grid)
(define (alorien-legal-offset pzzl lopolyn lolopos)
  (filter (lambda (x) (not (empty? x)))
          (map (lambda (z)
                 (local [(define leagl-lolopos
                           (filter (lambda (x) (not (empty? x)))
                                   (map (lambda (y)
                                          (filter (lambda (x)
                                                    (legal? pzzl z x)) y))
                                        lolopos)))]
                   (cond [(empty? leagl-lolopos)empty]
                         [else (superimpose pzzl z
                                            (first (first leagl-lolopos)))])))
               lopolyn)))
               
               
;; (neighbours my-state) produces a list of States in which one additional
;;       polyomino has been placed in the puzzle of the consumed State
;;       (my-state) and removed from the list of pieces of the consumed State
;;       (my-state) yet to be placed.
;; neighbours: State -> (listof State)
(define (neighbours my-state)
  (local [(define state-pzzl (state-puzzle my-state))
          (define state-pce (state-pieces my-state))
          (define pzzl-size (build-2dlist (length (first state-pzzl))
                                          (length state-pzzl)
                                          (lambda (x y) (make-pos x y))))]
    (foldr (lambda (x y)
             (append (map (lambda (a) (make-state a (rest state-pce)))
                          (alorien-legal-offset state-pzzl
                                                (all-orientations x)
                                                pzzl-size)) y))
           empty state-pce)))
