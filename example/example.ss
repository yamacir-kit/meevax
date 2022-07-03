(import (meevax foreign-function)
        (scheme base)
        (scheme process-context)
        (scheme write)
        (srfi 78))

; ------------------------------------------------------------------------------

(define dummy-procedure
  (foreign-function "build/libexample.so" "dummy_procedure"))

(check (foreign-function? dummy-procedure) => #t)

(check (dummy-procedure 'hoge 42 #(1 2 3) 3.14) => 43)

; ------------------------------------------------------------------------------

(define length-of-arguments
  (foreign-function "build/libexample.so" "length_of_arguments"))

(check (foreign-function? length-of-arguments) => #t)

(check (length-of-arguments 'hoge 42 #(1 2 3) 3.14) => 4)

; ------------------------------------------------------------------------------

(define make-hoge
  (foreign-function "build/libexample.so" "make_hoge"))

(define hoge?
  (foreign-function "build/libexample.so" "is_hoge"))

(define hoge-value
  (foreign-function "build/libexample.so" "hoge_value"))

(check (foreign-function? make-hoge) => #t)

(check (foreign-function? hoge?) => #t)

(check (foreign-function? hoge-value) => #t)

(define h (make-hoge 100))

(display h)
(newline)

(check (hoge? h) => #t)

(check (hoge-value h) => 100)

(exit (check-passed? 9))
