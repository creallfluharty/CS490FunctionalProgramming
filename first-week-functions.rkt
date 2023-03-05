#lang racket

(define (fizzbuzz [n 100])
  (for/list ([x (in-range 1 (add1 n))])
    (let ([m (string-append (if (= (modulo x 3) 0) "fizz" "") (if (= (modulo x 5) 0) "buzz" ""))])
    	(if (non-empty-string? m) m x))))

(define (quicksort lyst [lt <])
  (if (empty? lyst)
      lyst
      (let*-values ([(pivot) (first lyst)]
                    [(right left) (partition (curry lt pivot) (rest lyst))])
        `(,@(quicksort left) ,pivot ,@(quicksort right)))))

(define (fib k) ; please don't ask for the 0th term, I wanted to save a line :P
  (define (inner n fib-n-2 fib-n-1)
    (if (= n 1)
        fib-n-1
        (inner (sub1 n) fib-n-1 (+ fib-n-2 fib-n-1))))
  (inner k 0 1))

(fizzbuzz 20)
(quicksort '(3 1 1 1 6 8 6 3 4 6))
(fib 5)
