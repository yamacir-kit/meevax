; ---- SRFI-78 -----------------------------------------------------------------

(check-set-mode! 'report)

; ------------------------------------------------------------------------------

(check (eqv? 2 2.0) => #f)
(check (eqv? 0.0 +nan.0) => #f)

; (check (eqv? 1.0e0 1.0f0) => TODO) ; unspecified
(check (eqv? +nan.0 +nan.0) => #t) ; unspecified

; (check '#1=(a b . #1#)
;        '#2=(a b a b . #2#)           => #t)

(check (zero? 0/1) => #t)

(check (+ 1   1.0) => 2.0)
(check (+ 1.0 1  ) => 2.0)

(check (+ 1   1/2) => 3/2)
(check (+ 1.0 1/2) => 1.5)

(check (+ 1/2 1  ) => 3/2)
(check (+ 1/2 1.0) => 1.5)

(check (* 2   1/2) => 1  )
(check (* 2.0 1/2) => 1.0)

(check (floor-quotient  5  2) =>  2)
(check (floor-quotient -5  2) => -3)
(check (floor-quotient  5 -2) => -3)
(check (floor-quotient -5 -2) =>  2)

(check (floor-remainder  5  2) =>  1)
(check (floor-remainder -5  2) =>  1)
(check (floor-remainder  5 -2) => -1)
(check (floor-remainder -5 -2) => -1)

(check (truncate-quotient  5    2) =>  2)
(check (truncate-quotient -5    2) => -2)
(check (truncate-quotient  5   -2) => -2)
(check (truncate-quotient -5   -2) =>  2)
(check (truncate-quotient -5.0 -2) =>  2.0)

(check (truncate-remainder  5    2) =>  1)
(check (truncate-remainder -5    2) => -1)
(check (truncate-remainder  5   -2) =>  1)
(check (truncate-remainder -5   -2) => -1)
(check (truncate-remainder -5.0 -2) => -1.0)

(check (modulo  13  4) =>  1)
(check (modulo -13  4) =>  3)
(check (modulo  13 -4) => -3)
(check (modulo -13 -4) => -1)

(check (remainder  13  4)   =>  1)
(check (remainder -13  4)   => -1)
(check (remainder  13 -4)   =>  1)
(check (remainder -13 -4)   => -1)
(check (remainder -13 -4.0) => -1.0) ; inexact

(check (gcd 32 -36) => 4)
(check (gcd) => 0)

(check (lcm 32   -36) => 288)
(check (lcm 32.0 -36) => 288.0) ; inexact
(check (lcm) => 1)

(check (numerator (/ 6 4)) => 3)
(check (denominator (/ 6 4)) => 2)

(check
  (denominator
    (inexact (/ 6 4))) => 2.0)

(check (log 0.0) => -inf.0)

(check (sin          0)  (=> =) 0)
(check (sin (/ fl-pi 6)) (=> =) 0.5)
; (check (sin (/ fl-pi 4)) (=> =) 0.707107)
; (check (sin (/ fl-pi 3)) (=> =) 0.866025)
(check (sin (/ fl-pi 2)) (=> =) 1)
(check (sin    fl-pi)    (=> =) 0)

(check (atan  0.0  1.0) =>  0.000000)
; (check (atan  1.0  1.0) =>  0.785398)
; (check (atan  1.0  0.0) =>  1.570796)
; (check (atan  1.0 -1.0) =>  2.356194)
; (check (atan  0.0 -1.0) =>  3.141593)
; (check (atan -1.0 -1.0) => -2.356194)
; (check (atan -1.0  0.0) => -1.570796)
; (check (atan -1.0  1.0) => -0.785398)

(check (exact 0.333333333333) => 1/3)

(check (string? "Another example:\ntwo lines of test") => #t)
(check (string? "Here's test \
                   containing just one line") => #t)
(check (string? "\x03B1; is named GREEK SMALL LETTER ALPHA.") => #t)

; (define a "12345")
; (define b (string-copy "abcde"))
; (string-copy! b 1 a 0 2)
; (check b => "a12de")

(check (string=? (make-string 3) "") => #f)
(check (string=? (make-string 3) "   ") => #t)
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

(check (string-upcase   "AbdEgH") => "ABDEGH")
(check (string-downcase "AbdEgH") => "abdegh")

; ---- string-copy (substring) -------------------------------------------------

(check (string-copy "abcde")     => "abcde")
(check (string-copy "abcde" 1)   =>  "bcde")
(check (string-copy "abcde" 1 4) =>  "bcd" )

; ---- string-append -----------------------------------------------------------

(check (string-append)                   => "")
(check (string-append "abc")             => "abc")
(check (string-append "abc" "def")       => "abcdef")
(check (string-append "abc" "def" "ghi") => "abcdefghi")

; ---- string->list ------------------------------------------------------------

(check (string->list "abcde")     => (#\a #\b #\c #\d #\e))
(check (string->list "abcde" 1)   => (    #\b #\c #\d #\e))
(check (string->list "abcde" 1 4) => (    #\b #\c #\d    ))

; ---- string-fill! ------------------------------------------------------------

(let ((s "abcde")) (check (begin (string-fill! s #\x) s) => "xxxxx"))
(let ((s "abcde")) (check (begin (string-fill! s #\x 1) s) => "axxxx"))
(let ((s "abcde")) (check (begin (string-fill! s #\x 1 4) s) => "axxxe"))

(define integers
  (letrec ((next
             (lambda (n)
               (delay (cons n (next (+ n 1))))))) ; R7RS
    (next 0)))

(define head (lambda (stream) (car (force stream))))
(define tail (lambda (stream) (cdr (force stream))))

(define (stream-filter p? s)
  (delay-force
    (if (null? (force s))
        (delay '())
        (let ((h (car (force s)))
              (t (cdr (force s))))
          (if (p? h)
              (delay (cons h (stream-filter p? t)))
              (stream-filter p? t))))))

(check (head (tail (tail (stream-filter odd? integers)))) => 5)

(check
  (let ((path '())
        (c #f))
    (let ((add (lambda (s)
                 (set! path (cons s path)))))
      (dynamic-wind
        (lambda () (add 'connect))
        (lambda ()
          (add (call-with-current-continuation
                 (lambda (c0)
                   (set! c c0)
                   'talk1))))
        (lambda () (add 'disconnect)))
      (if (< (length path) 4)
          (c 'talk2)
          (reverse path))))

  => (connect talk1 disconnect
      connect talk2 disconnect))






; ---- 2.1. Identifiers --------------------------------------------------------

(check (symbol? '...) => #t)
(check (symbol? '+) => #t)
(check (symbol? '+soup+) => #t)
(check (symbol? '<=?) => #t)
(check (symbol? '->string) => #t)
(check (symbol? 'a34kTMNs) => #t)
(check (symbol? 'lambda) => #t)
(check (symbol? 'list->vector) => #t)
(check (symbol? 'q) => #t)
(check (symbol? 'V17a) => #t)
; (check (symbol? |two words|) => #t)
; (check (symbol? |two\x20;words|) => #t)
(check (symbol? 'the-word-recursion-has-many-meanings) => #t)

#!fold-case
#!no-fold-case

; ---- 2.2. Whitespace and comments --------------------------------------------

; #|
;   The FACT procedure computes the factorial
;   of a non-negative integer.
; |#
(define fact
  (lambda (n)
    (if (= n 0)
        #;(= n 1)
        1
        ;Base case: return 1
        (* n (fact (- n 1))))))

; ------------------------------------------------------------------------------
;  4.2.6. Dynamic bindings
; ------------------------------------------------------------------------------

; ---- make-parameter ----------------------------------------------------------

(define p1
  (make-parameter 1))

(check (p1)   => 1)
(check (p1 2) => 2)
(check (p1)   => 2)

(define p2
  (make-parameter "hoge"))

(parameterize ((p1 42)
               (p2 "fuga"))
  (print "p1 = " (p1)) (newline)
  (print "p2 = " (p2)) (newline)
  (check (p1) => 42)
  (check (p2) => "fuga")
  (list (p1) (p2)))

; ------------------------------------------------------------------------------
;  6.13.1 Input and output
; ------------------------------------------------------------------------------

; ---- get-output-string -------------------------------------------------------

(check
  (parameterize ((current-output-port (open-output-string)))
    (display "piece")
    (display " by piece ")
    (display "by piece.")
    (newline)
    (get-output-string (current-output-port)))

  => "piece by piece by piece.\n")




(define ffi.so (linker "libmeevax-test-foreign-function-interface.so"))

(define dummy-procedure
  (procedure ffi.so "dummy_procedure"))

(check
  (dummy-procedure "hello, world!\n" 42 '(1 . 2) #(1 2 3) 3.14)
  => 43)

(define-syntax (swap! x y)
  `(,let ((,value ,x))
     (,set! ,x ,y)
     (,set! ,y ,value)))

(check
  (let ((x 1)
        (y 2))
    (swap! x y)
    (cons x y)) => (2 . 1))

(check
  (let ((x 1)
        (y 2)
        (let '())
        (set! '())
        (value 42)
        )
    (swap! x y)
    (cons x y)) => (2 . 1))

(check
  (eval '(+ 1 2 3)
        (fork/csc
          (lambda (this) this))) => 6)

(define-syntax (increment x . n)
  (let ((n (if (pair? n) (car n) 1)))
    `(,begin (,set! ,x (,+ ,x ,n)) ,x)))


; ---- SRFI-78 -----------------------------------------------------------------

(check-report)

(exit (if (check-passed? check::correct) exit-success exit-failure))
