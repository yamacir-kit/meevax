(import (scheme base)
        (scheme char)
        (scheme process-context)
        (scheme write)
        (srfi 78)
        (srfi 211 explicit-renaming))

(define p1 (make-parameter 1))

(define p2 (make-parameter "hoge"))

(check (p1) => 1)

(check (p1 2) => 2)

(check (p1) => 2)

(parameterize ((p1 42)
               (p2 "fuga"))
  (display "p1 = ") (display (p1)) (newline)
  (display "p2 = ") (display (p2)) (newline)
  (check (p1) => 42)
  (check (p2) => "fuga")
  (list (p1) (p2)))

(check (string=? (make-string 3) "") => #f)

(check (string=? (make-string 3 #\a) "aaa") => #t)

(check (string=? (string) "") => #t)

(check (string=? (string #\h #\o #\g #\e) "hoge") => #t)

(check (string-length "") => 0)

(check (string-length "abc") => 3)

(check (string-ref "abc" 1) => #\b)

(define s "abc")

(string-set! s 1 #\x)

(check s => "axc")

(check (string=?  "abc" "abc") => #t)
(check (string<?  "abc" "bcd") => #t)
(check (string>?  "bcd" "abc") => #t)
(check (string<=? "abc" "abd") => #t)
(check (string>=? "abc" "aba") => #t)

(check (string-ci=?  "aBc" "AbC") => #t)
(check (string-ci<?  "aBc" "BcD") => #t)
(check (string-ci>?  "bCd" "AbC") => #t)
(check (string-ci<=? "aBc" "AbD") => #t)
(check (string-ci>=? "aBc" "AbA") => #t)

; ---- string-foldcase ---------------------------------------------------------

(check (string-upcase "AbdEgH") => "ABDEGH")

(check (string-downcase "AbdEgH") => "abdegh")

; ---- string-copy (substring) -------------------------------------------------

(check (string-copy "abcde") => "abcde")

(check (string-copy "abcde" 1) => "bcde")

(check (string-copy "abcde" 1 4) => "bcd" )

; ---- string-append -----------------------------------------------------------

(check (string-append) => "")

(check (string-append "abc") => "abc")

(check (string-append "abc" "def") => "abcdef")

(check (string-append "abc" "def" "ghi") => "abcdefghi")

; ---- string->list ------------------------------------------------------------

(check (string->list "abcde") => '(#\a #\b #\c #\d #\e))

(check (string->list "abcde" 1) => '(#\b #\c #\d #\e))

(check (string->list "abcde" 1 4) => '(#\b #\c #\d))

; ---- string-fill! ------------------------------------------------------------

(let ((s "abcde"))
  (check (begin (string-fill! s #\x) s) => "xxxxx"))

(let ((s "abcde"))
  (check (begin (string-fill! s #\x 1) s) => "axxxx"))

(let ((s "abcde"))
  (check (begin (string-fill! s #\x 1 4) s) => "axxxe"))

(define-syntax loop
  (er-macro-transformer
    (lambda (form rename compare)
      `(,(rename 'call/cc)
         (,(rename 'lambda) (exit)
           (,(rename 'let) ,(rename 'rec) () ,(cadr form) (,(rename 'rec))))))))

(define f
  (lambda ()
    (define x 0)

    (define let     3.14)
    (define call/cc 3.141)
    ; (define lambda  3.1415) ; IMPLEMENTATION MISS
    (define exit    3.14159)
    (define rec     3.141592)

    (loop
      (if (< 9 x)
          (begin (display "!")
                 (display exit)
                 (exit 42))
          (begin (display x)
                 (set! x (+ x 1)) )))))

; (define-syntax loop
;   (sc-macro-transformer
;     (lambda (form environment)
;      `(call-with-current-continuation
;         (lambda (exit)
;           (let loop ()
;            ,(make-syntactic-closure environment '(exit) (cadr form))
;             (loop)))))))

; ------------------------------------------------------------------------------

(check-report)

(exit (check-passed? 38))
