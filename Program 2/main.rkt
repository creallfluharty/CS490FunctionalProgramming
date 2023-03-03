#lang racket

(define (string->wordlist string)
  (regexp-split #rx"[^A-Za-zÀ-ž][^A-Za-zÀ-ž']*" string))

(define (get-frequency-table wordlist)
  (foldl
   (λ (word table) (hash-update table word add1 0))
   (hash)
   (map string-downcase wordlist)))

(define (get-subhash full-hash keys)
  (for/hash ([k (in-list keys)])
    (values k (hash-ref full-hash k))))

(define (get-relative-frequency-table frequency-table)
  (let ([total-words (for/sum ([count (in-hash-values frequency-table)]) count)])
    (for/hash ([(word count) (in-hash frequency-table)]) (values word (/ count total-words)))))

(define (get-author-profile author-name)
  (call-with-input-file (format "~a.txt" author-name)
    (λ (in-port)
      (list author-name
            (get-relative-frequency-table
             (get-frequency-table (string->wordlist (port->string in-port))))))))

(define (frequency-diff a-freq b-freq) (abs (log (/ a-freq b-freq) 10)))

(define (difference profile-a profile-b)
  (let*-values ([(common-keys) (filter (curry hash-has-key? (second profile-b)) (hash-keys (second profile-a)))]
                [(total-diff count) (for/fold ([sum 0] [count 0])
                                              ([word (in-list common-keys)])
                                      (values (+ sum (frequency-diff (hash-ref (second profile-a) word)
                                                                     (hash-ref (second profile-b) word)))
                                              (add1 count)))])
    (/ total-diff count)))  ; no disjoint sets, pls

(define (get-closest-match mystery authors)
  (argmin (curry difference mystery) authors))

(let ([labelled (list (get-author-profile "Lovecraft") (get-author-profile "Doyle"))]
      [unlabelled (list (get-author-profile "mystery1") (get-author-profile "mystery2"))])
  (for ([mystery (in-list unlabelled)])
    (printf "'~a' was most likely written by '~a'.~n" (first mystery) (first (get-closest-match mystery labelled)))))