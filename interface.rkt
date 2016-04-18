(module interface (lib "plt-pretty-big-text.ss" "lang")
  
  ;; DO NOT MODIFY THIS FILE!
  
  (require (lib "graphics.ss" "graphics"))
  (open-graphics)
  
  (define max-width 800)
  (define max-height 600)
  
  (define the-width #f)
  (define the-height #f)
  (define the-canvas #f)
  (define cols (list (list #\. (make-rgb 1 1 1))))
  
  (define count 0)
  
  ;; get-col: Character -> rgb
  ;; Get the colour associated with a given grid character, caching
  ;; colours in an association list so that they can be re-used.
  ;; Invent random colours for previously unseen characters.
  (define (get-col ch)
    (local
      [(define col (assq ch cols))]
      (begin
        (cond
          [(boolean? col)
           (begin
             (set! col (make-rgb (random) (random) (random)))
             (set! cols (cons (list ch col) cols))
             
             col)]
          [else (second col)]))))
  
  ;; get-canvas-size: Grid -> (list Nat Nat)
  ;; Determine the appropriate width and height for the canvas
  ;; from the side of a puzzle grid.
  (define (get-canvas-size grid)
    (local
      [(define gw (length (first grid)))
       (define gh (length grid))]
      (list
       (if (> gw gh) max-width (round (* max-height (/ gw gh))))
       (if (< gw gh) max-height (round (* max-width (/ gh gw)))))))
  
  ;; get-the-canvas: Grid -> Viewport
  ;; Get the drawing surface for the rest of the drawing operations.
  ;; Construct a new one if there isn't one yet or if the grid size
  ;; changes.
  (define (get-the-canvas grid)
    (cond
      [(not (and (equal? the-width (length (first grid)))
                 (equal? the-height (length grid))))
       (local
         [(define cs (get-canvas-size grid))]
         (begin
           (set! the-width (length (first grid)))
           (set! the-height (length grid))
           (set! the-canvas (open-viewport "Puzzle" (+ 40 (first cs))
                                           (+ 40 (second cs))))
           the-canvas))]
      [else the-canvas]))
  
  ;; draw-grid: Grid -> Void
  ;; Draw the puzzle grid to the current viewport.
  (define (draw-grid grid)
    (local
      [(define canvas (get-the-canvas grid))
       (define cs (get-canvas-size grid))
       
       (define dim (/ (first cs) the-width))
       
       (define line (draw-line canvas))
       (define rect (draw-solid-rectangle canvas))
       
       (define top 20)
       (define bottom (+ 20 (* dim the-height)))
       (define left 20)
       (define right (+ 20 (* dim the-width)))
       
       (define (grid-ref x y) (list-ref (list-ref grid y) x))
       (define (pt x y) (make-posn (+ 20 (* x dim)) (+ 20 (* y dim))))
       
       (define black (make-rgb 0 0 0))]
      
      (for-each 
       (lambda (y)
         (for-each 
          (lambda (x) 
            (local
              [(define c (grid-ref x y))
               (define col (get-col c))]
              (rect (pt x y) dim dim col)))
          (build-list the-width identity))) (build-list the-height identity))
      (for-each 
       (lambda (y)
         (for-each 
          (lambda (x) 
            (local
              [(define c (grid-ref x y))
               (define cr (if (< x (sub1 the-width)) 
                              (grid-ref (add1 x) y) 
                              'undef))
               (define cd (if (< y (sub1 the-height))
                              (grid-ref x (add1 y))
                              'undef))]
              (if (not (equal? c cr))
                  (line (pt (add1 x) y) (pt (add1 x) (add1 y)) black)
                  (void))
              (if (not (equal? c cd))
                  (line (pt x (add1 y)) (pt (add1 x) (add1 y)) black)
                  (void))))
          (build-list the-width identity))) (build-list the-height identity))
      
      (line (make-posn left top) (make-posn right top) black)
      (line (make-posn left top) (make-posn left bottom) black)
      (line (make-posn right top) (make-posn right bottom) black)
      (line (make-posn right bottom) (make-posn left bottom) black)))
  
  ;;;;;;;;;;;;;;;;;;;;;;
  ;; search: (X -> Boolean) (X -> (listof X)) (union (X -> Any) false) X 
  ;;         -> (union X false)
  ;; Solve a polyomino puzzle, given the current state of the
  ;; puzzle.  Use depth-first search with backtracking, assuming
  ;; that the search space is acyclic.  The parameters are:
  ;;  * at-end? -- a predicate that determines whether the given
  ;;      state is an end state of the search (i.e., a solution).
  ;;  * neighbours -- a function that maps a current state to a list
  ;;      of neighbouring states.
  ;;  * viz -- either a function that does something with the current
  ;;      state (e.g., draws it on the screen), or false to indicate 
  ;;      nothing should be done.
  ;;  * a-state -- the initial state.
  
  (define (search at-end? neighbours viz a-state)
    (local
      [;; find-route: X -> (union X false)
       ;; Search outward from this configuration to see if there's a path
       ;; to a solution.
       (define (find-route a-state)
         (begin
           (if (not (boolean? viz)) (viz a-state))
           (cond
             [(at-end? a-state) a-state]
             [else (find-route/list (neighbours a-state))])) )
       
       ;; find-route/list: (listof X) -> (union X false)
       ;; Search outward from every configuration in the passed-in list of
       ;; configurations.  If any one of them leads to a solution, stop and
       ;; produce that solution.  Produce false if you run out of options.
       (define (find-route/list lostate)
         (cond
           [(empty? lostate) false]
           [else
            (local
              [(define cur (find-route (first lostate)))]
              (cond
                [(not (boolean? cur)) cur]
                [else (find-route/list (rest lostate))]))]))]
      (find-route a-state)))
  
  ;;;;;;;;;;;;;;;;;;;;;;
  ;; lists-equiv?: (listof X) (listof X) -> boolean
  ;; Given two lists l1 and l2, determine whether the lists are essentially
  ;; the same up to reordering.  This function will work as long as one of
  ;; the two lists contains no duplicates.
  
  ;; Examples:
  ; (check-expect (lists-equiv? '("1" "2" "3") '("2" "3" "1")) true)
  ; (check-expect (lists-equiv? '(1 2 3 4) '(2 3 4 5)) false)
  
  (define (lists-equiv? l1 l2)
    ;; The approach is a bit sneaky, but very succinct.  Check that
    ;; every element of l1 appears somewhere in l2 (in terms of equal?),
    ;; and that every elements of l2 appears somewhere in l1.
    (and (= (length l1) (length l2))
         (andmap (lambda (x1) (ormap (lambda (x2) (equal? x1 x2)) l2)) l1)
         (andmap (lambda (x2) (ormap (lambda (x1) (equal? x1 x2)) l1)) l2)))
  
  ;;;;;;;;;;;;;;;;;;;;;;
  ;; strlist->grid: (listof String) -> (listof (listof Character))
  ;; Turn a list of (equal length) strings into a grid by breaking each
  ;; string into its constituent characters.
  
  ;; Examples:
  ;(check-expect (strlist->grid empty) empty)
  ;(check-expect (strlist->grid '("abc" "def")) '((#\a #\b #\c) (#\d #\e #\f)))
  
  (define (strlist->grid los)
    (map string->list los))
  
  (define tetrominoes-uc
    (map strlist->grid
         '(("AAAA")
           ("BBB" "B..")
           ("CCC" ".C.")
           ("DD" "DD")
           (".EE" "EE."))))
  
  (define tetrominoes-lc
    (map strlist->grid
         '(("aaaa")
           ("bbb" "b..")
           ("ccc" ".c.")
           ("dd" "dd")
           (".ee" "ee."))))
  
  ;; A single set of tetrominoes cannot tile a rectangle.
  ;; But *two* sets together can tile both 4x10 and 5x8 
  ;; rectangles.
  (define double-tetrominoes
    (append tetrominoes-uc
            tetrominoes-lc))
  
  ;; A complete set of 12 pentominoes tiles a 6x10 rectangle,
  ;; a 5x12 rectangle, a 4x15 rectangle, and a 3x20 rectangle.
  
  (define p/F (strlist->grid '(".FF" "FF." ".F.")))
  (define p/I (strlist->grid '("IIIII")))
  (define p/L (strlist->grid '("LLLL" "...L")))
  (define p/P (strlist->grid '("PPP" ".PP")))
  (define p/N (strlist->grid '(".NNN" "NN..")))
  (define p/T (strlist->grid '("TTT" ".T." ".T.")))
  (define p/U (strlist->grid '("U.U" "UUU")))
  (define p/V (strlist->grid '("V.." "V.." "VVV")))
  (define p/W (strlist->grid '("WW." ".WW" "..W")))
  (define p/X (strlist->grid '(".X." "XXX" ".X.")))
  (define p/Y (strlist->grid '(".Y.." "YYYY")))
  (define p/Z (strlist->grid '("ZZ." ".Z." ".ZZ")))
  
  (define pentominoes
    (list p/F p/I p/L p/P p/N p/T p/U p/V p/W p/X p/Y p/Z))
  
  ;; Construct ten copies of the Y pentomino, each with a different letter
  ;; name.  These can fill a 5x10 rectangle.
  
  (define 10y
    (map strlist->grid
         (map (lambda (x) (list (string x x x x) (string #\. x #\. #\.)))
              (string->list "ABCDEFGHIJ"))))
  
  ;; What follows is a complete set of partial puzzles for one 6x10 
  ;; pentomino solution.
  
  (define pent-solution 
    (list "FFIIIIILZZ" "VFFYLLLLZN" "VFYYYYXZZN"
          "VVVTWXXXNN" "PPPTWWXUNU" "PPTTTWWUUU"))
  
  (define pent-grid-12
    (strlist->grid pent-solution))
  (define pent-pieces-12 
    (list))
  
  (define pent-grid-11
    (strlist->grid
     (list "FFIIIIIL.." "VFFYLLLL.N" "VFYYYYX..N"
           "VVVTWXXXNN" "PPPTWWXUNU" "PPTTTWWUUU")))
  (define pent-pieces-11 
    (list p/Z))
  
  (define pent-grid-10
    (strlist->grid
     (list "FFIIIIIL.." "VFF.LLLL.N" "VF....X..N"
           "VVVTWXXXNN" "PPPTWWXUNU" "PPTTTWWUUU")))
  (define pent-pieces-10
    (list p/Y p/Z))
  
  (define pent-grid-9
    (strlist->grid
     (list "FFIIIIIL.." "VFF.LLLL.N" "VF.......N"
           "VVVTW...NN" "PPPTWW.UNU" "PPTTTWWUUU")))
  (define pent-pieces-9
    (list p/X p/Y p/Z))
  
  (define pent-grid-8
    (strlist->grid
     (list "FFIIIIIL.." "VFF.LLLL.N" "VF.......N"
           "VVVT....NN" "PPPT...UNU" "PPTTT..UUU")))
  (define pent-pieces-8
    (list p/W p/X p/Y p/Z))
  
  (define pent-grid-7
    (strlist->grid
     (list "FFIIIIIL.." ".FF.LLLL.N" ".F.......N"
           "...T....NN" "PPPT...UNU" "PPTTT..UUU")))
  (define pent-pieces-7
    (list p/V p/W p/X p/Y p/Z))
  
  (define pent-grid-6
    (strlist->grid
     (list "FFIIIIIL.." ".FF.LLLL.N" ".F.......N"
           "...T....NN" "PPPT....N." "PPTTT.....")))
  (define pent-pieces-6
    (list p/U p/V p/W p/X p/Y p/Z))
  
  (define pent-grid-5
    (strlist->grid
     (list "FFIIIIIL.." ".FF.LLLL.N" ".F.......N"
           "........NN" "PPP.....N." "PP........")))
  (define pent-pieces-5
    (list p/T p/U p/V p/W p/X p/Y p/Z))
  
  (define pent-grid-4
    (strlist->grid
     (list "FFIIIIIL.." ".FF.LLLL.." ".F........"
           ".........." "PPP......." "PP........")))
  (define pent-pieces-4
    (list p/N p/T p/U p/V p/W p/X p/Y p/Z))
  
  (define pent-grid-3
    (strlist->grid
     (list "FFIIIIIL.." ".FF.LLLL.." ".F........"
           ".........." ".........." "..........")))
  (define pent-pieces-3
    (list p/P p/N p/T p/U p/V p/W p/X p/Y p/Z))
  
  (define pent-grid-2
    (strlist->grid
     (list "FFIIIII..." ".FF......." ".F........"
           ".........." ".........." "..........")))
  (define pent-pieces-2
    (list p/L p/P p/N p/T p/U p/V p/W p/X p/Y p/Z))
  
  (define pent-grid-1
    (strlist->grid
     (list "FF........" ".FF......." ".F........"
           ".........." ".........." "..........")))
  (define pent-pieces-1
    (list p/I p/L p/P p/N p/T p/U p/V p/W p/X p/Y p/Z))
  
  (define pent-grid-0
    (strlist->grid
     (list ".........." ".........." ".........."
           ".........." ".........." "..........")))
  (define pent-pieces-0
    (list p/F p/I p/L p/P p/N p/T p/U p/V p/W p/X p/Y p/Z))
  
  (provide search 
           draw-grid
           
           strlist->grid
           tetrominoes-uc
           tetrominoes-lc
           double-tetrominoes
           p/F p/I p/L p/P p/N p/T p/U p/V p/W p/X p/Y p/Z
           pentominoes
           10y
           
           pent-solution
           pent-pieces-12 pent-grid-12
           pent-pieces-11 pent-grid-11
           pent-pieces-10 pent-grid-10
           pent-pieces-9 pent-grid-9
           pent-pieces-8 pent-grid-8
           pent-pieces-7 pent-grid-7
           pent-pieces-6 pent-grid-6
           pent-pieces-5 pent-grid-5
           pent-pieces-4 pent-grid-4
           pent-pieces-3 pent-grid-3
           pent-pieces-2 pent-grid-2
           pent-pieces-1 pent-grid-1
           pent-pieces-0 pent-grid-0
           
           lists-equiv?))
