#lang racket

(require data/either)
(require data/monad)
(require "app-state.rkt")


(define (stack-top stack)
  (if (not (empty? stack))
      (success (first stack))
      (failure "Can't get the top of an empty stack!")))

(define (stack-pop stack)
  (if (not (empty? stack))
      (success (list (first stack) (rest stack)))
      (failure "Can't pop from an empty stack!")))

(define (stack-push item stack)
  (cons item stack))

(define (stack-show stack)
  (format "[~a]" (string-join (map number->string stack))))

(define (stack-op op stack)
  (do [(list b r1) <- (stack-pop stack)]
    [(list a r2) <- (stack-pop r1)]
    [result <- (op a b)]
    (success (stack-push result r2))))

(define (stack-duplicate-top stack)
  (do [top <- (stack-top stack)]
    (success (stack-push top stack))))

(define (safe-div a b)
  (if (equal? b 0)
      (failure "Cannot divide by zero!")
      (success (/ a b))))

(define (prompt p)
  (printf p)
  (read-line (current-input-port)))

(define (command-handler command stack)
  (case command
    [("ADD") (chain app-continue (stack-op (compose success +) stack))]
    [("SUB") (chain app-continue (stack-op (compose success -) stack))]
    [("MUL") (chain app-continue (stack-op (compose success *) stack))]
    [("DIV") (chain app-continue (stack-op safe-div stack))]
    [("CLR") (app-continue '())]
    [("SHOW") (app-continue stack (stack-show stack))]
    [("TOP") (do [top <- (stack-top stack)] (app-continue stack top))]
    [("SIZ") (app-continue stack (length stack))]
    [("DUP") (chain app-continue (stack-duplicate-top stack))]
    [("END") (app-exit stack)]
    [else (let ([num (string->number command)])
            (if num
                (app-continue (stack-push num stack))
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

(let ([exit-state (calculator-loop '())])
  (printf "Exiting normally. The stack was ~a" (stack-show (app-state-stack exit-state))))
