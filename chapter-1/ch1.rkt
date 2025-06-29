#lang racket
;; Ex 1.2.
;(/ (+ (+ 4 5) (- 2 (- 3 (+ 6 (/ 4 5)))))
;  (* 3 (- 6 2)(- 2 7)))
;; -0.2466

;; Ex 1.3.
(define (square x) (* x x))
;(+ (square 5) (square 6))
(define (sum_of_squares a b)
  (+ (* a a) (* b b)))
;(sum_of_squares 5 6)
(define (sum_of_largest_two a b c)
  (cond ((and (>= a b) (>= b c)) (sum_of_squares a b))
        ((>= a c) (sum_of_squares a c))
        (else (sum_of_squares b c))))
; (sum_of_largest_two 5 6 7)

;; Ex 1.4.
(define (abs-a-b a b)
  ((if (> b 0) + -) a b))
;(abs-a-b 4 -2)

;; Ex 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))
;; Applicative => Never really
;; Normal => Returns 0 

;; Newton's Approximations for Square-roots

(define (sqrt-iter guess x)
  (new-if (error guess x)
      guess
      (sqrt-iter (improved-guess guess x)
      x)))

(define (error guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improved-guess guess x)
  (average guess (/ x guess)))

(define (average x y) (/ (+ x y) 2))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; Ex 1.6: New IF
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
