;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname polyominoes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "interface.rkt")

;; Uncomment the following line if you want to use
;; the examples in kanoodle.rkt
;; (require "kanoodle.rkt")

;; A Grid is a (listof (listof Char))
;; requires: both inner and outer lists of Grid are non-empty

(define-struct pos (x y))
;; A Pos is a (make-pos Int Int)

(define-struct state (puzzle pieces))
;; A State is a (make-state Grid (listof Grid))

;; build-2dlist: Nat Nat (Nat Nat -> X) -> (listof (listof X))
;; requires: w,h > 0
;; Construct a two-dimensional grid (represented as a list of lists),
;; in which each element is some function of its coordinates. A sort
;; of 2D analogue of build-list.

;; Examples:
(check-expect (build-2dlist 1 1 list) (list (list (list 0 0))))
(check-expect (build-2dlist 3 2 *) '((0 0 0) (0 1 2)))

(define (build-2dlist w h fn)
  (build-list h (lambda (y) (build-list w (lambda (x) (fn x y))))))

;; Tests:
(check-expect (build-2dlist 5 5 +)
              '((0 1 2 3 4) (1 2 3 4 5) (2 3 4 5 6) (3 4 5 6 7) (4 5 6 7 8)))
(check-expect (build-2dlist 1 3 list)
              '(((0 0)) ((0 1)) ((0 2))))
(check-expect (build-2dlist 3 1 list)
              '(((0 0) (1 0) (2 0))))

;; all-positions: Nat Nat -> (listof Pos)
;; requires w,h > 0
;; Produce the set of all legal grid positions for
;; a grid of the passed-in (input) dimendions.

;; Examples:
(check-expect (all-positions 1 1) (list (make-pos 0 0)))
(check-expect
 (lists-equiv?
  (all-positions 2 2)
  (list (make-pos 0 0) (make-pos 1 0) (make-pos 0 1) (make-pos 1 1)))
 true)

(define (all-positions w h)
  (foldr append empty (build-2dlist w h make-pos)))

;; Tests:
(check-expect (all-positions 1 1) (list (make-pos 0 0)))
(check-expect (all-positions 1 3)
              (list (make-pos 0 0) (make-pos 0 1) (make-pos 0 2)))
(check-expect (all-positions 3 1)
              (list (make-pos 0 0) (make-pos 1 0) (make-pos 2 0)))
(check-expect
 (lists-equiv?
  (all-positions 3 4)
  (list (make-pos 0 0) (make-pos 1 0) (make-pos 2 0)
        (make-pos 0 1) (make-pos 1 1) (make-pos 2 1)
        (make-pos 0 2) (make-pos 1 2) (make-pos 2 2)
        (make-pos 0 3) (make-pos 1 3) (make-pos 2 3)))
 true)

;; all-orientations: Grid -> (listof Grid)
;; Produce a list of up to eight orientations of the input grid, taking into
;; account all the possible rotations by multiples of 90 degrees and reflections.

;; Examples:
(check-expect (all-orientations '((#\a))) (list '((#\a))))
(check-expect (all-orientations '((#\a #\a) (#\a #\a) (#\. #\a)))
              (list (list (list #\. #\a) (list #\a #\a) (list #\a #\a))
                    (list (list #\a #\a #\a) (list #\. #\a #\a))
                    (list (list #\a #\a) (list #\a #\a) (list #\a #\.))
                    (list (list #\a #\a #\.) (list #\a #\a #\a))
                    (list (list #\a #\a) (list #\a #\a) (list #\. #\a))
                    (list (list #\a #\a #\a) (list #\a #\a #\.))
                    (list (list #\a #\.) (list #\a #\a) (list #\a #\a))
                    (list (list #\. #\a #\a) (list #\a #\a #\a))))

(define (all-orientations grid)
  (local [;; transpose: Grid -> Grid
          ;; Produce a new grid whose rows are the columns of th input grid.
          (define (transpose grid)
            (cond [(empty? (first grid)) empty]
                  [else (cons (map first grid) (transpose (map rest grid)))]))

          ;; rotate: Grid -> Grid
          ;; Rotate the passed-in grid 90 degrees counter-clockwise.
          (define (rotate grid)
            (reverse (transpose grid)))

          ;; insert-uniq: Any List -> List
          ;; Insert the new values into the given list if it isn't there already.
          ;; If it is, the original list is produce unchanged.
          (define (insert-uniq el lst)
            (cond [(member? el lst) lst]
                  [else (cons el lst)]))

          ;; all-rotations: Grid -> (listof Grid)
          ;; Produce a list of all the (unique) copies of the
          ;; input grid, rotated by multiples of 90 degrees.
          (define (all-rotations grid)
            (local [(define 90degrees (rotate grid))
                    (define 180degrees (rotate 90degrees))
                    (define 270degrees (rotate 180degrees))]
              (foldr insert-uniq empty
                     (list grid 90degrees 180degrees 270degrees))))]
    (foldr insert-uniq (all-rotations grid) (all-rotations (reverse grid)))))

;; Tests:
;; Eight orientations:
(check-expect
 (lists-equiv?
  (all-orientations '((#\a #\a #\a) (#\. #\. #\a)))
  (list '((#\a #\a #\a) (#\. #\. #\a)) '((#\. #\a) (#\. #\a) (#\a #\a))
        '((#\a #\. #\.) (#\a #\a #\a)) '((#\a #\a) (#\a #\.) (#\a #\.))
        '((#\a #\a #\a) (#\a #\. #\.)) '((#\a #\.) (#\a #\.) (#\a #\a))
        '((#\. #\. #\a) (#\a #\a #\a)) '((#\a #\a) (#\. #\a) (#\. #\a))))
 true)
;; Four orientations (one reflection symmetry):
(check-expect
 (lists-equiv?
  (all-orientations '((#\a #\a #\a) (#\a #\. #\a)))
  (list '((#\a #\a #\a) (#\a #\. #\a)) '((#\a #\a) (#\. #\a) (#\a #\a))
         '((#\a #\. #\a) (#\a #\a #\a)) '((#\a #\a) (#\a #\.) (#\a #\a))))
 true)
;; Two orientations (two reflection symmetries):
(check-expect
 (lists-equiv?
  (all-orientations '((#\a #\a #\a)))
  (list '((#\a #\a #\a)) '((#\a) (#\a) (#\a))))
 true)
;; Four orientations (one 180 degree rotation symmetry):
(check-expect
 (lists-equiv?
  (all-orientations '((#\a #\a #\.) (#\.#\a #\a)))
  (list '((#\a #\a #\.) (#\. #\a #\a)) '((#\. #\a #\a) (#\a #\a #\.))
        '((#\a #\.) (#\a #\a) (#\. #\a)) '((#\. #\a) (#\a #\a) (#\a #\.))))
 true)
;; Two orientations (90 degree rotation symmetry):
(check-expect
 (lists-equiv?
  (all-orientations '((#\a #\a #\. #\a) (#\. #\. #\. #\a) (#\a #\. #\. #\.) (#\a #\. #\a #\a)))
  (list '((#\a #\a #\. #\a) (#\. #\. #\. #\a) (#\a #\. #\. #\.) (#\a #\. #\a #\a))
        '((#\a #\. #\a #\a) (#\a #\. #\. #\.) (#\. #\. #\. #\a) (#\a #\a #\. #\a))))
 true)
;; One orientation (lots of symmetries):
(check-expect
 (all-orientations '((#\a #\a) (#\a #\a)))
 (list '((#\a #\a) (#\a #\a))))

;; first-empty-pos: (listof (listof Char)) -> (anyof Pos false)
;; Find the position associated with the first empty (#\.) cell
;; in this grid, or produce false if there are no empty cells.

;; Examples:
(check-expect (first-empty-pos (strlist->grid '(".ab" "de." "ghi")))
              (make-pos 0 0))
(check-expect (first-empty-pos (strlist->grid '("abc" "de." "ghi")))
              (make-pos 2 1))
(check-expect (first-empty-pos (strlist->grid '("abcdef" "ghijkl")))
              false)

(define (first-empty-pos grid)
  (first-pos-pred grid (lambda (ch) (char=? ch #\.))))

;; Tests:
(check-expect (first-empty-pos (make-empty-grid 10 10))
              (make-pos 0 0))
(check-expect (first-empty-pos (strlist->grid '("abcdef" "ghijkl")))
              false)
(check-expect (first-empty-pos (strlist->grid '("abcd" "efgh" "i..j")))
              (make-pos 1 2))

;; first-pos-pred: Grid (Char -> Bool) -> Pos
          ;; Examine the contents of every cell in the grid. Produce as output
          ;; the coordinates of the first cell that passes the given predicate.

;; Examples:
(check-expect (first-pos-pred (strlist->grid '("1234" "456a" "7890"))
                              char-alphabetic?)
              (make-pos 3 1))

(define (first-pos-pred grid pred?)
  (local [(define (first-pred-in-list L)
            (cond
              [(empty? L) false]
              [(pred? (grid-ref grid (first L))) (first L)]
              [else (first-pred-in-list (rest L))]))]
    (first-pred-in-list (all-positions (length (first grid)) (length grid)))))

;; Tests:
(check-expect (first-pos-pred (strlist->grid '("sTRING" "beans!"))
                              char-lower-case?)
              (make-pos 0 0))
(check-expect (first-pos-pred (strlist->grid '("STRING" "beans!"))
                              char-lower-case?)
              (make-pos 0 1))
(check-expect (first-pos-pred (strlist->grid '("11111" "11111" "11111" "111x1" "11111"))
                              (lambda (ch) (char=? ch #\x)))
              (make-pos 3 3))

;; make-empty-grid: Nat Nat -> Puzzle
;; Construct an initial empty puzzle grid of the given dimensions.

;; Examples:
(check-expect (make-empty-grid 1 1) '((#\.)))
(check-expect (make-empty-grid 4 3)
              '((#\. #\. #\. #\.)
                (#\. #\. #\. #\.)
                (#\. #\. #\. #\.)))

(define (make-empty-grid width height)
  (make-list height (make-list width #\.)))

;; Tests:
(check-expect (make-empty-grid 1 1) '((#\.)))
(check-expect (make-empty-grid 10 1)
              '((#\. #\. #\. #\. #\. #\. #\. #\. #\. #\.)))
(check-expect (make-empty-grid 1 10)
              '((#\.) (#\.) (#\.) (#\.) (#\.) (#\.) (#\.) (#\.) (#\.) (#\.)))
(check-expect (make-empty-grid 4 3)
              '((#\. #\. #\. #\.)
                (#\. #\. #\. #\.)
                (#\. #\. #\. #\.)))

;; grid-ref: Grid Pos -> Any
;; Lookup the cell at the given position in the grid. If the position is legal
;; (i.e., it lies within the bounds of the grid), produce the contents of the
;; cell. If it's out of bounds, produce the Symbol 'undef (which is therefore
;; presumed not to reside anywhere within the grid.)

;; Examples:
(check-expect (grid-ref '((a b c) (d e f) (g h i)) (make-pos 1 1)) 'e)

(define (grid-ref grid p)
       (local [(define width (length (first grid)))
               (define height (length grid))
               ;; list-lookup: (listof Any) Nat -> (anyof Any false)
               ;; Produces the pos item in lst or false if index not valid in lst
               (define (list-lookup lst pos)
                 (cond [(empty? lst) false]
                       [(= pos 0) (first lst)]
                       [else (list-lookup (rest lst) (sub1 pos))]))]
         (cond [(< (pos-x p) 0) 'undef]
               [(>= (pos-x p) width) 'undef]
               [(< (pos-y p) 0) 'undef]
               [(>= (pos-y p) height) 'undef]
               [else (list-lookup (list-lookup grid (pos-y p)) (pos-x p))])))

;; Tests:
(check-expect (grid-ref '((a b) (c d)) (make-pos -1 1)) 'undef)
(check-expect (grid-ref '((a b) (c d)) (make-pos 3 1)) 'undef)
(check-expect (grid-ref '((a b) (c d)) (make-pos 1 -5)) 'undef)
(check-expect (grid-ref '((a b) (c d)) (make-pos 1 7)) 'undef)
(check-expect (grid-ref '((a b c d) (e f g h) (i j k l) (m n o p))
                        (make-pos 3 1)) 'h)

;; superimpose: Grid Grid Pos -> Grid
;; Carry out the superposition of top onto base, by shifting top
;; by the given offset and copying its non-empty cells over.

;; Examples:
(check-expect (superimpose (make-empty-grid 3 3) p/X (make-pos 0 0)) p/X)
(check-expect (superimpose (strlist->grid '("aaa" "..." "b.."))
                           p/X (make-pos 1 0))
              (strlist->grid '("aaX" ".XX" "b.X")))

(define (superimpose puzz top offset)
  (build-2dlist
   (length (first puzz)) (length puzz) ;; The width and height of the grid.
   (lambda (x y)
     (local
       [;; Extract the relevant characters from the lower and
        ;; upper grids and make a decision based on them.
        (define under (grid-ref puzz (make-pos x y)))
        (define over (grid-ref top (make-pos (- x (pos-x offset))
                                             (- y (pos-y offset)))))]
       (cond
         [(symbol? over) under]      ;; Nope, top square is out of bounds.
         [(char=? over #\.) under]   ;; Top square is empty, don't overwrite.
         [else over])))))

;; Tests:
(check-expect (superimpose (strlist->grid '("..B." "CCB."))
                           (strlist->grid '("AAAA" ".A.."))
                           (make-pos 1 0))
              (strlist->grid '(".AAA" "CCA.")))
(check-expect (superimpose (strlist->grid '("ABCD" "EFGH" "IJKL"))
                           (strlist->grid '(".XX." "XXXX" ".XX."))
                           (make-pos -40 -40))
              (strlist->grid '("ABCD" "EFGH" "IJKL")))
(check-expect (superimpose (strlist->grid '("AAAA" "AAA." "AAAA"))
                           (strlist->grid '("BBBBBBBB"))
                           (make-pos 3 1))
              (strlist->grid '("AAAA" "AAAB" "AAAA")))
(check-expect (superimpose
               (strlist->grid '("AAAAA" "AA.AA" "A...A" "AA.AA" "AAAAA"))
               p/X
               (make-pos 1 1))
              (strlist->grid '("AAAAA" "AAXAA" "AXXXA" "AAXAA" "AAAAA")))

;; neighbours: State -> (listof State)
;; Move from a current state to all possible states that can follow
;; directly from it, where a move consists of occupying the uppermost
;; leftmost unused cell in the grid with a legally placed polyomino
;; from the list of those that are available.

;; Examples:
(check-expect
 (lists-equiv?
  (neighbours
   (make-state (make-empty-grid 2 2)
               '(((#\X #\X)))))
  (list
   (make-state '((#\X #\X) (#\. #\.)) empty)
   (make-state '((#\X #\.) (#\X #\.)) empty)))
 true)

(define (neighbours s)
  (local
    [;; Extract the curent puzzle and unplaced polyominoes for convinience.
     (define puzz (state-puzzle s))
     (define width (length (first puzz)))
     (define height (length puzz))
     (define pieces (state-pieces s))

     ;; can-superimpose?: Grid Pos -> Boolean
     ;; Consider the possible move of superimposing the top puzzlem,
     ;; shifted over by the given translation vector (offset) onto
     ;; the base. Determine whether the superposition would be legal.
     (define (can-superimpose? top offset)
       (local
         [;; check-one: Char Char -> Boolean
          ;; Assuming two character are both in bounds, figure out whether
          ;; the top one can legally be placed on top of the base.
          (define (check-one c-base c-top)
            (cond
              ;; If the base Pos is out of bounds, the top
              ;; had better be empty in the same square.
              [(symbol? c-base) (char=? c-top #\.)]
              ;; If the top square is being used,
              ;; make sure the base is empty there.
              [(not (char=? c-top #\.)) (char=? c-base #\.)]
              [else true]))]
         (andmap
          (lambda (p)
            (check-one
             (grid-ref puzz
                       (make-pos (+ (pos-x offset) (pos-x p))
                                 (+ (pos-y offset) (pos-y p))))
             (grid-ref top p)))
          (all-positions (length (first top)) (length top)))))

     ;; first-valid-offset: Pos Grid -> Pos
     (define (first-valid-offset empty-pos top)
       (local
         [(define pt (first-pos-pred top (lambda (ch) (not (char=? ch #\.)))))]
         (make-pos (- (pos-x empty-pos) (pos-x pt))
                   (- (pos-y empty-pos) (pos-y pt)))))

     ;; test-orientations: (listof Grid) (listof Grid) -> (listof State)
     ;; For a given polyomino, determine the list of all new puzzles that
     ;; can result by placing this polyomino in any orientation at the
     ;; empty position in the existing puzzle.
     (define (test-orientations oris all-but)
       (cond [(empty? oris) empty]
             [else
              (local
                [(define ori (first oris))
                 (define fe (first-valid-offset first-empty ori))]
                (cond
                  [(can-superimpose? ori fe)
                   (cons (make-state (superimpose puzz ori fe) all-but)
                         (test-orientations (rest oris) all-but))]
                  [else (test-orientations (rest oris) all-but)]))]))

     ;; Compute the first empty position in the puzzle.
     (define first-empty (first-empty-pos puzz))]

    (cond
      [(boolean? first-empty) empty]
      [else
       (foldr append empty
              (map (lambda (poly)
                     (local
                       [(define all-but (remove poly pieces))]
                       (test-orientations (all-orientations poly) all-but)))
                   pieces))])))
  
;; solve-puzzle: Grid (listof Grid) Sym -> (anyof (listof Str) false)
;; Solve a polyomino puzzle, given the initial empty (or partially filled 
;; in) grid, a set of pieces that must be placed, and a Symbol indicating
;; what visualization style to use.  Legal viz styles are 'interactive
;; (draw every step of the search), 'at-end (just draw the solution, if one
;; is found), or 'offline (don't draw anything).  Produce either the solved
;; Grid (converted to a list of Strings, just for convenience) or false if
;; no solution exists.

;; solve-puzzle: Grid (listof Grid) Sym -> (anyof (listof Str) false)
;; requires: viz-style is one of {'interactive, 'at-end or 'offline}

;; Some Examples are included below after the solve-puzzle function definition.

;; DO NOT MODIFY THIS CODE
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

;; Examples:
;; (The examples are not provided in check-expect form.  They're meant to
;; demonstrate typical uses of the function, but we don't want to them to
;; open interactive visualizations every time you start the program.)

;; Solve offline (i.e. work like a normal Scheme function).
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'offline)

;; Display the result graphically, if a solution is found.
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'at-end)

;; Display every step of the search as it progresses.
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'interactive)