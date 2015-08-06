#lang racket

(provide non-zero
         any?
         all?
         allindex
         rowindex
         colindex
         boxindex
         coord->box)


(module+ test
  (require rackunit))


;; removes 0s from lst
(define (non-zero lst)
  (filter-not (lambda (x) (= 0 x)) lst))

(module+ test
  (check-equal? (non-zero '(1 0 2 0 3)) '(1 2 3)))


;; returns #t if any item in lst is true, otherwise returns #f
(define (any? lst)
  (if (null? lst) #f
      (or (first lst) (any? (rest lst)))))

(module+ test
  (check-equal? (any? (list #f #t #f)) #t)
  (check-equal? (any? (list #f #f #f)) #f))


;; returns #t if all items in lst are true, otherwise returns #f
(define (all? lst)
  (if (null? lst) #t
      (and (first lst) (all? (rest lst)))))

(module+ test
  (check-equal? (all? (list #f #t #f)) #f)
  (check-equal? (all? (list #f #f #f)) #f)
  (check-equal? (all? (list #t #t #t)) #t))


(define (allindex)
  (apply append (map (lambda (x) (map (lambda (y) (list x y)) (range 1 10))) (range 1 10))))

(module+ test
  (check-equal? (length (allindex)) (* 9 9))
  (check-equal? (first (allindex)) '(1 1))
  (check-equal? (last (allindex)) '(9 9))
  (check-equal? (second (allindex)) '(1 2)))


(define (rowindex row)
  (map (lambda (x) (list row x)) (range 1 10)))

(module+ test
  (check-equal? (first (rowindex 1)) '(1 1))
  (check-equal? (second (rowindex 1)) '(1 2))
  (check-equal? (first (rowindex 2)) '(2 1))
  (check-equal? (length (rowindex 1)) 9))


(define (colindex col)
  (map (lambda (x) (list x col)) (range 1 10)))

(module+ test
  (check-equal? (first (colindex 1)) '(1 1))
  (check-equal? (second (colindex 1)) '(2 1))
  (check-equal? (first (colindex 2)) '(1 2))
  (check-equal? (length (colindex 1)) 9))


(define (boxindex n)
  (let-values ([(q r) (quotient/remainder (sub1 n) 3)])
    (let ((qlow (add1 (* q 3)))
          (qhigh (add1 (* (add1 q) 3)))
          (rlow (add1 (* r 3)))
          (rhigh (add1 (* (add1 r) 3))))
      (apply append
             (map (lambda (x) (map (lambda (y)
                                     (list x y)) (range rlow rhigh))) (range qlow qhigh))))))

(module+ test
  (check-equal? (first (boxindex 1)) '(1 1))
  (check-equal? (second (boxindex 1)) '(1 2))
  (check-equal? (fourth (boxindex 1)) '(2 1))
  (check-equal? (ninth (boxindex 1)) '(3 3))
  (check-equal? (first (boxindex 2)) '(1 4))
  (check-equal? (length (boxindex 1)) 9))


;; given coordinate (row, col), returns the box index it belongs to.
(define (coord->box row col)
  (+ 1
     (* 3 (quotient (sub1 row) 3))
     (quotient (sub1 col) 3)))

(module+ test
  (check-equal? (coord->box 1 1) 1)
  (check-equal? (coord->box 3 3) 1)
  (check-equal? (coord->box 1 4) 2)
  (check-equal? (coord->box 9 9) 9))
