#lang racket
(define (string-repeat str n)
  (if (= n 1) str
      (string-append str (string-repeat str (- n 1)))))

(define (fence n)
  (define (helper p s)
    (cond
      [(> p (round (+ 1 (log n)))) s]
      [(= p (round (+ 1 (log n)))) (helper (+ p 1) (string-append "{-" s "-}"))]
      [else (helper (+ p 1) (string-append "-" s "-"))]))
    (helper 1 (string-append ">" (number->string n) "<")))

(define (series a b n)
  (define (helper p q count)
    (if (= count 1) p
        (helper q (+ p q) (- count 1))))
  (helper a b n))

(define (lucas n)
  (series 2 1 n))

(define (fibonacci n)
  (series 1 1 n))

(define (summed-member n)
  (+ (lucas n) (fibonacci n)))

(define (nth-fibonacci-sum n)
  (define (helper n res)
    (if (> n 0) (helper (- n 1) (+ res (fibonacci n)))
        res))
  (helper n 0))

(define (nth-lucas-sum n)
  (define (helper n res)
    (if (> n 0) (helper (- n 1) (+ res (lucas n)))
        res))
  (helper n 0))

(define (lucas-fib-diff n)
  (- (lucas n) (fibonacci n)))