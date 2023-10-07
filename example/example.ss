(import (only (meevax procedure) procedure)
        (scheme base)
        (scheme process-context)
        (scheme write)
        (srfi 78))

; ------------------------------------------------------------------------------

(define dummy-procedure
  (procedure "build/libexample.so" "dummy_procedure"))

(check (procedure? dummy-procedure) => #t)

(check (dummy-procedure 'hoge 42 #(1 2 3) 3.14) => 43)

; ------------------------------------------------------------------------------

(define length-of-arguments
  (procedure "build/libexample.so" "length_of_arguments"))

(check (procedure? length-of-arguments) => #t)

(check (length-of-arguments 'hoge 42 #(1 2 3) 3.14) => 4)

; ------------------------------------------------------------------------------

(define make-hoge
  (procedure "build/libexample.so" "make_hoge"))

(define hoge?
  (procedure "build/libexample.so" "is_hoge"))

(define hoge-value
  (procedure "build/libexample.so" "hoge_value"))

(check (procedure? make-hoge) => #t)

(check (procedure? hoge?) => #t)

(check (procedure? hoge-value) => #t)

(define h (make-hoge 100))

(display h)
(newline)

(check (hoge? h) => #t)

(check (hoge-value h) => 100)

(exit (check-passed? 9))
