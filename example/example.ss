(import (only (meevax procedure) procedure)
        (scheme base)
        (scheme process-context)
        (scheme write)
        (srfi 78))

(display (get-environment-variable "LD_LIBRARY_PATH"))
(newline)

; ------------------------------------------------------------------------------

(define dummy-procedure
  (procedure "libexample.so" 'dummy_procedure))

(check (procedure? dummy-procedure) => #t)

(check (dummy-procedure 'hoge 42 #(1 2 3) 3.14) => 43)

; ------------------------------------------------------------------------------

(define arity
  (procedure "libexample.so" 'arity))

(check (procedure? arity) => #t)

(check (arity 'hoge 42 #(1 2 3) 3.14) => 4)

; ------------------------------------------------------------------------------

(define make-hoge
  (procedure "libexample.so" 'make_hoge))

(define hoge?
  (procedure "libexample.so" 'is_hoge))

(define hoge-value
  (procedure "libexample.so" 'hoge_value))

(check (procedure? make-hoge) => #t)

(check (procedure? hoge?) => #t)

(check (procedure? hoge-value) => #t)

(define h (make-hoge 100))

(display h)
(newline)

(check (hoge? h) => #t)

(check (hoge-value h) => 100)

(exit (check-passed? 9))
