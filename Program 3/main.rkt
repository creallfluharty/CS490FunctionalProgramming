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
  (success (format "[~a]" (string-join (map number->string stack)))))

(define (stack-display-result f stack)
  (do [result <- (f stack)]
    (begin
      (displayln result)
      (success stack))))

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
  (case command
    [("ADD") (stack-op (compose success +) stack)]
    [("SUB") (stack-op (compose success -) stack)]
    [("MUL") (stack-op (compose success *) stack)]
    [("DIV") (stack-op safe-div stack)]
    [("CLR") (stack-clear stack)]
    [("SHOW") (stack-display-result stack-show stack)]
    [("TOP") (stack-display-result stack-top stack)]
    [("SIZ") (stack-display-result (compose success length) stack)]
    [("DUP") (stack-duplicate-top stack)]
    [("END") (failure "normal exit")] ; TODO handle this better
    [else (let ([num (string->number command)])
            (if num
                (stack-push num stack)
                (failure (format "Unrecognized command '~a'!" command))))]))

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