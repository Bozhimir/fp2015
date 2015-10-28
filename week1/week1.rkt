#lang racket
(define (cut-last-digit n)
  (remainder n 10))

(define (cut-number-from-last-digit n)
  (quotient n 10))

(define (product-of-digits n)
  (if (= n 0)
       1
       (* (cut-last-digit n) (product-of-digits (cut-number-from-last-digit n)))))

(define (square n)
  (* n n))

(define (circle? circle-x circle-y radius point-x point-y)
  (<= (+ (square (- point-x circle-x)) (square (- point-y circle-y))) (square radius)))

(define (half-perimeter a b c)
  (/ (+ a b c) 2))

(define (area a b c)
  (sqrt (* (half-perimeter a b c) (- (half-perimeter a b c) a) (- (half-perimeter a b c) b) (- (half-perimeter a b c) c))))
