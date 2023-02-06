#lang racket

(define (fizzbuzz [n 100])
  (define (inner x)
    (define m (string-append (if (= (modulo x 3) 0) "fizz" "") (if (= (modulo x 5) 0) "buzz" "")))
    (if (non-empty-string? m) m x))
  
  (map inner (range 1 (add1 n))))

(define (quicksort lyst [lt <])
  (if (empty? lyst)
      lyst
      (let ([pivot (first lyst)])
        (append
         (quicksort (filter (λ (x) (lt x pivot)) (rest lyst)))
         (list pivot)
         (quicksort (filter (λ (x) (lt pivot x)) (rest lyst)))))))

(define (fib k) ; please don't ask for the 0th term, I wanted to save a line :P
  (define (inner n fib-n-2 fib-n-1)
    (if (= n 1)
        fib-n-1
        (inner (sub1 n) fib-n-1 (+ fib-n-2 fib-n-1))))
  (inner k 0 1))