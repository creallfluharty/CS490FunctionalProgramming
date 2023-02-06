#lang racket

; Reads all lines from "GradebookData.txt" where each line is in format
;   <first-name> <last-name> <quiz1> ... <quiz 5> <exam 1> ... <exam 3>
; and writes
;   <last-name> <first-name> <class-score> <letter-grade>
; to "GradebookOutput.txt" sorted by (last-name, first-name)

(define (avg lyst) ; we could just use (sum lyst) and (length lyst) but that's two passes >:(
  (define (inner lyst sum len)
    (if (empty? lyst)
        (/ sum len)
        (inner (rest lyst) (+ (first lyst) sum) (add1 len))))
  (inner lyst 0 0))

(define (get-course-grade quizzes exams)
  (+ (* 35/20 (avg quizzes)) (* 65/100 (avg exams))))

(define (get-letter-grade grade)
(define (get-letter-grade grade)
  (cond [(>= grade 93) "A"] ; special conditions since there are no A+s, or F±s
        [(< grade 60) "F"]
        [else (let*-values ([(tens ones) (quotient/remainder (floor grade) 10)]
                            [(letter-pos) (- tens 6)])
                (string-append (substring "DCBA" letter-pos (add1 letter-pos))
                               (if (< ones 3) "-" "") (if (<= 7 ones) "+" "")))]))

(define (parse-line line)
  (let*-values ([(split-line) (string-split line)]
                [(name grade-strings) (split-at split-line 2)]
                [(grades) (map string->number grade-strings)]
                [(quizzes exams) (split-at grades 5)]
                [(grade) (get-course-grade quizzes exams)]
                [(letter-grade) (get-letter-grade grade)])
    (list (reverse name) (real->decimal-string grade 2) letter-grade)))

(define (list<? a b atom<?)
  (cond [(atom<? (first a) (first b)) #t]
        [(atom<? (first b) (first a)) #f]
        [else (list<? (rest a) (rest b) atom<?)]))

(define class-data (map parse-line (port->lines (open-input-file "GradebookData.txt"))))
(define sorted-class-data (sort class-data (λ (a b) (list<? (first a) (first b) string<?))))
(with-output-to-file "GradebookOutput.txt" #:exists 'replace
  (λ () (for ([student-data sorted-class-data])
    (apply (λ (name grade letter-grade) (displayln (string-join `(,@name ,grade ,letter-grade) " "))) student-data))))