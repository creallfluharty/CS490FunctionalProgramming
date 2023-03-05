#lang racket

(define (string->wordlist string)
  (regexp-match* #rx"[A-Za-zÀ-ž][A-Za-zÀ-ž']*" string))

(define (get-frequencies words)
  (for/fold ([count 0] [table (hash)])
            ([word (in-list words)])
    (values (add1 count) (hash-update table (string-downcase word) add1 0))))

(struct profile (name word-count frequencies))

(define (get-profile author-name)
  (call-with-input-file (format "~a.txt" author-name)
    (λ (in-port)
      (let-values ([(word-count frequencies) (get-frequencies (string->wordlist (port->string in-port)))])
        (profile author-name word-count frequencies)))))

(define (difference profile-a profile-b)
  (define (frequency-diff a-freq b-freq) (abs (log (/ a-freq b-freq) 10)))
  (let*-values ([(total-diff common-word-count)
                 (for/fold ([sum 0] [common-word-count 0])
                           ([word (in-hash-keys (profile-frequencies profile-a))]
                            #:when (hash-has-key? (profile-frequencies profile-b) word))
                   (values (+ sum (frequency-diff
                                   (/ (hash-ref (profile-frequencies profile-a) word)
                                      (profile-word-count profile-a))
                                   (/ (hash-ref (profile-frequencies profile-b) word)
                                      (profile-word-count profile-b))))
                           (add1 common-word-count)))])
    (/ total-diff common-word-count)))  ; no disjoint sets, pls

(define (get-closest-match mystery authors)
  (argmin (curry difference mystery) authors))

(let ([labelled (list (get-profile "Lovecraft") (get-profile "Doyle"))]
      [unlabelled (list (get-profile "mystery1") (get-profile "mystery2"))])
  (for ([mystery (in-list unlabelled)])
    (printf "'~a' was most likely written by '~a'.~n" (profile-name mystery) (profile-name (get-closest-match mystery labelled)))))

