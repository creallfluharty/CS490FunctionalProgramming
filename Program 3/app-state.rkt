#lang racket

(require data/maybe)
(require data/monad)

(struct app-state (tag output stack)
  #:transparent
  #:methods gen:monad
  [(define (chain f x)
     (case (app-state-tag x)
       [(exit) x]
       [(continue) 
        (do [out <- (app-state-output x)]
          (displayln out)) ; this could just be a chain if not for naming conflicts :(
        (f (app-state-stack x))]))])

(define (app-exit stack [output nothing])
  (app-state 'exit (if (nothing? output) output (just output)) stack))

(define (app-exit? m)
  (and (app-state? m) (equal? (app-state-tag m) 'exit)))

(define (app-continue stack [output nothing])
  (app-state 'continue (if (nothing? output) output (just output)) stack))

(define (app-continue? m)
  (and (app-state? m) (equal? (app-state-tag m) 'continue)))

(provide
 app-exit
 app-exit?
 app-continue
 app-continue?
 app-state-stack)
