(import (only (meevax procedure) procedure)
        (scheme base)
        (scheme process-context)
        (scheme write)
        (srfi 78))

(display (get-environment-variable "LD_LIBRARY_PATH"))
(newline)

(define libexample
  (cond-expand (darwin "libexample.dylib")
               (linux "libexample.so")))

(define dummy-procedure
  (procedure libexample 'dummy_procedure))

(check (procedure? dummy-procedure) => #t)

(check (dummy-procedure 'hoge 42 #(1 2 3) 3.14) => 43)

(define argument-length
  (procedure libexample 'argument_length))

(check (procedure? argument-length) => #t)

(check (argument-length 'hoge 42 #(1 2 3) 3.14) => 4)

(define make-hoge
  (procedure libexample 'make_hoge))

(define hoge?
  (procedure libexample 'is_hoge))

(define hoge-value
  (procedure libexample 'hoge_value))

(check (procedure? make-hoge) => #t)

(check (procedure? hoge?) => #t)

(check (procedure? hoge-value) => #t)

(define h (make-hoge 100))

(display h)
(newline)

(check (hoge? h) => #t)

(check (hoge-value h) => 100)

(exit (check-passed? 9))
