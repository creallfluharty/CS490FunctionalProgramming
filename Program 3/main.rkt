#lang racket

(require data/either)
(require data/monad)

(define (stack-top stack)
  (if (not (empty? stack))
      (success (first stack))
      (failure "Can't get the top of an empty stack!")))

(define (stack-pop stack)
  (if (not (empty? stack))
      (success (list (first stack) (rest stack)))
      (failure "Can't pop from an empty stack!")))

(define (stack-push item stack)
  (success (cons item stack)))

(define (stack-show stack)
  (printf "[~a]~n" (string-join (map number->string stack)))
  (success stack))

(define (stack-op op stack)
  (do [(list b r1) <- (stack-pop stack)]
      [(list a r2) <- (stack-pop r1)]
      [result <- (op a b)]
    (stack-push result r2)))

(define (stack-clear stack)
  (success '()))

(define (stack-duplicate-top stack)
  (do [top <- (stack-top stack)]
    (stack-push top stack)))

(define (safe-div a b)
  (if (equal? b 0)
      (failure "Cannot divide by zero!")
      (success (/ a b))))

(define (prompt p)
  (printf p)
  (read-line (current-input-port)))

(define (command-handler command stack)
  (do [f <- (case command
                [("ADD") (success (curry stack-op (compose success +)))]
                [("SUB") (success (curry stack-op (compose success -)))]
                [("MUL") (success (curry stack-op (compose success *)))]
                [("DIV") (success (curry stack-op safe-div))]
                [("CLR") (success stack-clear)]
                [("SHOW") (success stack-show)]
                [("TOP") (success stack-top)]
                [("SIZ") (success (compose success length))]
                [("DUP") (success stack-duplicate-top)]
                [("END") (failure "normal exit")] ; TODO handle this better
                [else (let ([num (string->number command)])
                        (if num
                            (success (curry stack-push num))
                            (failure (format "Unrecognized command '~a'!" command))))])]
        [new-stack <- (f stack)]
      (success new-stack)))

(define (commands-handler commands stack)
  (if (empty? commands)
      (success stack)
      (do [new-stack <- (command-handler (first commands) stack)]
        (commands-handler (rest commands) new-stack))))

(define (calculator-loop stack)
  (do (define line (prompt "Please enter a command: "))
    (define commands (string-split line))
    [result <- (commands-handler commands stack)]
    (calculator-loop result)))

(calculator-loop '())