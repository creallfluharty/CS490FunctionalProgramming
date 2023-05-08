#lang racket

(define (string->words string)
  (regexp-match* #rx"[A-Za-zÀ-ž][A-Za-zÀ-ž']*" string))

(define (get-first-line string)
  (read-line (open-input-string string)))

(define (normalize-words words stop-words)
  (filter-map
   (λ (word)
     (let ([lowercase (string-downcase word)])
       (and (not (set-member? stop-words lowercase)) lowercase)))
   words))

(define (get-frequencies words)
  (for/fold ([count 0] [table (hash)])
            ([word (in-list words)])
    (values (add1 count) (hash-update table (string-downcase word) add1 0))))

(struct profile (name word-count frequencies first-line))
(struct diff (words frequency) #:transparent)

(define (difference profile-a profile-b)
  (define (frequency-diff a-freq b-freq) (abs (log (/ a-freq b-freq) 10)))
  (let*-values
      ([(total-diff common-word-count)
        (for/fold ([sum 0] [common-word-count 0])
                  ([word (in-hash-keys (profile-frequencies profile-a))]
                   #:when (hash-has-key? (profile-frequencies profile-b) word))
          (values (+ sum (frequency-diff
                          (/ (hash-ref (profile-frequencies profile-a) word)
                             (profile-word-count profile-a))
                          (/ (hash-ref (profile-frequencies profile-b) word)
                             (profile-word-count profile-b))))
                  (add1 common-word-count)))])
    (diff common-word-count total-diff)))

(define (make-profile name contents stop-words)
  (let*-values
      ([(first-line) (get-first-line contents)]
       [(words) (normalize-words (string->words contents) stop-words)]
       [(word-count frequencies) (get-frequencies words)])
    (profile name word-count frequencies first-line)))

(define (load-profile file-name stop-words)
  (make-profile file-name (call-with-input-file file-name port->string) stop-words))

(define (load-cached-profile file-name stop-words)
  (with-handlers ([exn:fail:filesystem:errno? (λ(e) (load-profile file-name stop-words))])
    (call-with-input-file (format "~a.cache" file-name) (λ (port) (apply profile (read port))))))

(define (write-profile-cache profile)
  (with-handlers ([exn:fail:filesystem? (λ(e) #f)])
    (call-with-output-file (format "~a.cache" (profile-name profile))
      (λ (port) (write (list
                        (profile-name profile)
                        (profile-word-count profile)
                        (profile-frequencies profile)
                        (profile-first-line profile)) port))
      #:exists 'error)))

(define (get-profiles stop-words)
  (filter-map (λ (file-path)
                (let ([file-name (path->string file-path)])
                  (and (not (string-suffix? file-name ".cache"))
                       (load-cached-profile file-name stop-words))))
              (directory-list "Files" #:build? #t)))

(define (difference<? a b)
  (cond [(> (diff-words a) (diff-words b)) #t]  ; maximize the number of words in common
        [(> (diff-words b) (diff-words a)) #f]
        [else (< (diff-frequency a) (diff-frequency b))]))  ; minimize the difference in frequency of said words

(define (main)
  (define stop-words (list->set (call-with-input-file "stop_words_english.txt" port->lines)))
  (define profiles (get-profiles stop-words))
  (define (input-loop)
    (display "Please enter a query: ")
    (let ([line (read-line)])
      (unless (eof-object? line)
        (displayln "Below are the files in my database which match most closely:")
        (let* ([profile (make-profile "user-input" line stop-words)]
               [differences (map (λ (p) (cons (difference profile p) p)) profiles)]
               [sorted-differences (sort differences (λ (a b) (difference<? (car a) (car b))))])
          (for ([d (in-list sorted-differences)]
                #:unless (equal? (diff-words (car d)) 0))
            (displayln (profile-name (cdr d)))
            (displayln (profile-first-line (cdr d))))
          (input-loop)))))
  (input-loop)
  (for ([profile (in-list profiles)])
    (write-profile-cache profile)))

(main)