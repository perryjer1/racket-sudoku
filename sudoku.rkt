#lang racket

(provide read-puzzle
         list-slice
         get-row
         solved?
         solve)


(module+ test
  (require rackunit))

(require "util.rkt")


(module+ test
 (define my-puzzle
   '((0 0 3 0 2 0 6 0 0)
     (9 0 0 3 0 5 0 0 1)
     (0 0 1 8 0 6 4 0 0)
     (0 0 8 1 0 2 9 0 0)
     (7 0 0 0 0 0 0 0 8)
     (0 0 6 7 0 8 2 0 0)
     (0 0 2 6 0 9 5 0 0)
     (8 0 0 2 0 3 0 0 9)
     (0 0 5 0 1 0 3 0 0)))

 (define my-puzzle-solved
   '((4 8 3 9 2 1 6 5 7)
     (9 6 7 3 4 5 8 2 1)
     (2 5 1 8 7 6 4 9 3)
     (5 4 8 1 3 2 9 7 6)
     (7 2 9 5 6 4 1 3 8)
     (1 3 6 7 9 8 2 4 5)
     (3 7 2 6 8 9 5 1 4)
     (8 1 4 2 5 3 7 6 9)
     (6 9 5 4 1 7 3 8 2))))


;; returns the (row, col) item of the puzzle
(define (locate puzzle row col)
  (list-ref (list-ref puzzle (sub1 row)) (sub1 col)))

(module+ test
  (check-equal? (locate my-puzzle 1 1) 0)
  (check-equal? (locate my-puzzle 1 3) 3)
  (check-equal? (locate my-puzzle 2 1) 9)
  (check-equal? (locate my-puzzle 7 7) 5)
  (check-equal? (locate my-puzzle 9 9) 0))


;; same as locate but takes a pair (row col) as idx
(define (locate-idx puzzle idx)
  (locate puzzle (first idx) (second idx)))

(module+ test
  (check-equal? (locate-idx my-puzzle '(1 1)) 0)
  (check-equal? (locate-idx my-puzzle '(1 3)) 3)
  (check-equal? (locate-idx my-puzzle '(2 1)) 9)
  (check-equal? (locate-idx my-puzzle '(7 7)) 5)
  (check-equal? (locate-idx my-puzzle '(9 9)) 0))


;; returns all the numbers not in items (1 to 9)
(define (missing items)
  (remove* (non-zero items) (range 1 10)))

(module+ test
  (check-equal? (missing '(1 2 3 4 5)) '(6 7 8 9))
  (check-equal? (missing '(0 1 0 2 5 4 3)) '(6 7 8 9))
  (check-equal? (missing (range 1 10)) null))


;; predicate to test if anything is missing from items
(define (missing? items)
  (not (null? (missing items))))

(module+ test
  (check-equal? (missing? '(1 2 3 4 5)) #t)
  (check-equal? (missing? (range 1 10)) #f))


;; Puzzle accessors
;;
;; Puzzles have rows, columns and boxes, numbered according to the diagram
;; below (box numbers are inside).
;;
;;              columns
;;          1 2 3 4 5 6 7 8 9
;;
;;      1   + + + * * * + + +
;;      2   + 1 + * 2 * + 3 +
;;      3   + + + * * * + + +
;;      4   * * * + + + * * *
;; rows 5   * 4 * + 5 + * 6 *
;;      6   * * * + + + * * *
;;      7   + + + * * * + + +
;;      8   + 7 + * 8 * + 9 +
;;      9   + + + * * * + + +


;; returns the elements of the ith row as a list
(define (get-row puzzle i)
  (list-ref puzzle (sub1 i)))

(module+ test
  (check-equal? (get-row my-puzzle 1) '(0 0 3 0 2 0 6 0 0))
  (check-equal? (get-row my-puzzle 9) '(0 0 5 0 1 0 3 0 0)))


;; returns the elements of the ith column as a list
(define (get-col puzzle i)
  (apply list (map (lambda (x) (list-ref x (sub1 i))) puzzle)))

(module+ test
  (check-equal? (get-col my-puzzle 1) '(0 9 0 0 7 0 0 8 0))
  (check-equal? (get-col my-puzzle 9) '(0 1 0 0 8 0 0 9 0)))


;; returns a list of items in lst from index start to stop
(define (list-slice lst start stop)
  (take (list-tail lst start) (- stop start)))

(module+ test
  (check-equal? (list-slice (range 1 10) 0 3) '(1 2 3))
  (check-equal? (list-slice (range 1 10) 3 6) '(4 5 6))
  (check-equal? (list-slice (range 1 10) 6 9) '(7 8 9)))


;; returns the elements of the ith box as a list
(define (get-box puzzle i)
  (let-values ([(q r) (quotient/remainder (sub1 i) 3)])
    (let* ([row1 (add1 (* 3 q))]
           [row2 (add1 (* 3 (add1 q)))]
           [col1 (* 3 r)]
           [col2 (* 3 (add1 r))]
           [rows (map (lambda (x) (get-row puzzle x)) (range row1 row2))]
           [cols (map (lambda (x) (list-slice x col1 col2)) rows)])
      (apply append cols))))

(module+ test
  (check-equal? (get-box my-puzzle 1) '(0 0 3 9 0 0 0 0 1))
  (check-equal? (get-box my-puzzle 2) '(0 2 0 3 0 5 8 0 6))
  (check-equal? (get-box my-puzzle 9) '(5 0 0 0 0 9 3 0 0)))

(module+ test
  (check-equal? (missing (get-box my-puzzle 2)) '(1 4 7 9)))


;; returns a new puzzle with location (row, col) set to val
(define (set puzzle row col val)
  (define (setcol lst col val)
    (append (take lst (sub1 col)) (list val) (list-tail lst col)))
  (if (null? puzzle) null
      (if (= 1 row)
          (cons (setcol (first puzzle) col val) (set (rest puzzle) (sub1 row) col val))
          (cons (first puzzle) (set (rest puzzle) (sub1 row) col val)))))

(module+ test
  (check-equal? (locate (set my-puzzle 1 1 1) 1 1) 1)
  (check-equal? (locate (set my-puzzle 1 1 2) 1 1) 2)
  (check-equal? (locate my-puzzle 1 1) 0))


;; returns #t if the puzzle is solved, otherwise returns #f
(define (solved? puzzle)
  (cond
    ((null? puzzle) #f)
    (else
     (and
      (not (any? (map missing? (map (lambda (x) (get-row puzzle x)) (range 1 10)))))
      (not (any? (map missing? (map (lambda (x) (get-col puzzle x)) (range 1 10)))))
      (not (any? (map missing? (map (lambda (x) (get-box puzzle x)) (range 1 10)))))))))

(module+ test
  (check-equal? (solved? my-puzzle) #f)
  (check-equal? (solved? my-puzzle-solved) #t))


;; returns a list of numbers that can legally be put in
;; location (i,j). Returns null if (i,j) is already filled.
(define (valid-moves puzzle i j)
  (if (eq? 0 (locate puzzle i j))
      (let ((row-vals (non-zero (get-row puzzle i)))
            (col-vals (non-zero (get-col puzzle j)))
            (box-vals (non-zero (get-box puzzle (coord->box i j)))))
        (missing (append row-vals col-vals box-vals)))
      null))

(module+ test
  (check-equal? (valid-moves my-puzzle 1 5) null)
  (check-equal? (valid-moves my-puzzle 1 4) '(4 9))
  (check-equal? (valid-moves my-puzzle 2 5) '(4 7))
  (check-equal? (valid-moves my-puzzle 3 5) '(7 9))
  (check-equal? (valid-moves my-puzzle 1 6) '(1 4 7)))


;; converts a list of indexes ((i1,j1), ..., (in,jn)) to a
;; list of valid moves for each index ((a, ..., b), ..., (c, ..., d))
(define (idx->moves puzzle idx)
  (map (lambda (x) (valid-moves puzzle (car x) (cadr x))) idx))

(module+ test
  (check-equal? (idx->moves my-puzzle (boxindex 2))
                '((4 9) () (1 4 7) () (4 7) () () (7 9) ())))


;; counts the number of times x occurs in lst
(define (count-occur x lst)
  (cond
    ((null? lst) 0)
    ((member x (car lst)) (add1 (count-occur x (cdr lst))))
    (else (count-occur x (cdr lst)))))

(module+ test
  (check-equal? (count-occur 1 (idx->moves my-puzzle (boxindex 2))) 1)
  (check-equal? (count-occur 4 (idx->moves my-puzzle (boxindex 2))) 3)
  (check-equal? (count-occur 7 (idx->moves my-puzzle (boxindex 2))) 3)
  (check-equal? (count-occur 9 (idx->moves my-puzzle (boxindex 2))) 2))


;; returns the location to put x if it is uniquely determined
(define (find-if-solved-1 x lst idx)
  (if (not (= 1 (count-occur x lst)))
      null
      (cond
        ((null? lst) null)
        ((member x (car lst)) (car idx))
        (else (find-if-solved-1 x (cdr lst) (cdr idx))))))

(module+ test
  (check-equal? (find-if-solved-1 1 (idx->moves my-puzzle (boxindex 2)) (boxindex 2)) '(1 6))
  (check-equal? (find-if-solved-1 2 (idx->moves my-puzzle (boxindex 2)) (boxindex 2)) null)
  (check-equal? (find-if-solved-1 4 (idx->moves my-puzzle (boxindex 2)) (boxindex 2)) null))


(define (find-if-solved miss lst idx)
  (cond
    ((null? miss) null)
    ((not (null? (find-if-solved-1 (car miss) lst idx)))
     (list (find-if-solved-1 (car miss) lst idx) (car miss)))
    (else (find-if-solved (cdr miss) lst idx))))

(module+ test
  (check-equal? (find-if-solved '(1 4 7 9) (idx->moves my-puzzle (boxindex 2)) (boxindex 2))
                '((1 6) 1))
  (check-equal? (find-if-solved '(4 7 9) (idx->moves my-puzzle (boxindex 2)) (boxindex 2))
                null))


(define (solve-1 puzzle loc typefun idxfun)
  (cond
    ((or (null? loc) (null? puzzle)) null)
    (else
     (let* ((i (car loc))
            (miss (missing (typefun puzzle i)))
            (idx (idxfun i))
            (sol (find-if-solved miss (idx->moves puzzle idx) idx)))
       (cond
         ((not (null? sol)) (set puzzle (caar sol) (cadr (car sol)) (cadr sol)))
         (else (solve-1 puzzle (cdr loc) typefun idxfun)))))))

(module+ test
  (let ((rowsol (solve-1 my-puzzle (range 1 10) get-row rowindex))
        (colsol (solve-1 my-puzzle (range 1 10) get-col colindex))
        (boxsol (solve-1 my-puzzle (range 1 10) get-box boxindex)))
    (check-equal? (locate rowsol 1 6) 1)
    (check-equal? (locate colsol 3 1) 2)
    (check-equal? (locate boxsol 2 2) 6)))


;; gets the loction of the first unsolved entry in puzzle
(define (first-unsolved puzzle)
  (define (check puzzle idx)
    (cond
      ((null? idx) null)
      ((= 0 (locate-idx puzzle (car idx))) (car idx))
      (else (check puzzle (cdr idx)))))
  (check puzzle (allindex)))

(module+ test
  (check-equal? (first-unsolved my-puzzle) '(1 1)))


;; Helper for the recursion in guess. For each move in moves, this sets
;; the idx in puzzle to that move and calls solve to see if the move is
;; correct. If it is, the puzzle is solved, otherwise it tries the next
;; move.
(define (guess-helper puzzle idx moves)
  (cond
    ((null? moves) null)
    (else
     (let ((sol (solve (set puzzle (first idx) (second idx) (car moves)))))
       (cond
         ((null? sol) (guess-helper puzzle idx (cdr moves)))
         (else sol))))))

(define (guess puzzle)
  (let* ((unsolved (first-unsolved puzzle))
         (moves (valid-moves puzzle (first unsolved) (second unsolved))))
    (cond
      ((null? moves) null)
      (else (guess-helper puzzle unsolved moves)))))


;; return the first non null element in lst, if there is one
(define (first-notnull lst)
  (cond
    ((null? lst) null)
    ((null? (car lst)) (first-notnull (cdr lst)))
    (else (car lst))))

(module+ test
  (check-equal? (first-notnull '(() () (1 2))) '(1 2)))


;; Solve the puzzle!
;;
;; The basic strategy is to fill in one entry and then solve that
;; new puzzle. This is done by filling in an entry that is uniquely
;; determined by the other existing entries. If none can be filled
;; that way, then a guess is made. If the guess is found to be
;; incorrect, the solver backs up to where the guess was made and
;; tries another move.
;;
;; Below, on each step, the rows, columns and boxes are examined
;; so in reality up to three entries can be filled on each step.
;;
(define (solve puzzle)
  (let* ((sol-row (solve-1 puzzle (range 1 10) get-row rowindex))
         (sol-col (solve-1 sol-row (range 1 10) get-col colindex))
         (sol-box (solve-1 sol-col (range 1 10) get-box boxindex))
         (sol (first-notnull (list sol-row sol-col sol-box))))
    (cond
      ((solved? sol) sol)            ; If solved, we are done.
      ((null? sol) (guess puzzle))   ; null? means solution failed to make progess so we must guess
      (else (solve sol)))))          ; Not solved and 

(module+ test
  (check-equal? (solve my-puzzle) my-puzzle-solved))


(define (read-row row)
  (for/list ((s row)) (string->number (make-string 1 s))))

(module+ test
  (check-equal? (read-row "003201090") '(0 0 3 2 0 1 0 9 0)))


(define (read-puzzle in)
  (map (lambda (i) (read-row (read-line in))) (range 1 10)))

(module+ test
  (let ((in (open-input-string "01\n02\n03\n04\n05\n06\n07\n08\n09\n")))
    (check-equal? (read-puzzle in) '((0 1) (0 2) (0 3) (0 4) (0 5) (0 6) (0 7) (0 8) (0 9)))))


(define (valid-puzzle? puzzle)
  (cond
    ((null? puzzle) #t)
    ((not (= 9 (length (car puzzle)))) #f)
    (else (valid-puzzle? (cdr puzzle)))))

(module+ test
  (check-equal? (valid-puzzle? my-puzzle) #t)
  (let ((in (open-input-string "01\n02\n03\n04\n05\n06\n07\n08\n09\n")))
    (check-equal? (valid-puzzle? (read-puzzle in)) #f)))
