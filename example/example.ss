(import (meevax foreign-function-interface))

(define dummy-procedure (foreign-function "build/libexample.so" "dummy_procedure"))

(check (foreign-function? dummy-procedure) => #t)
(check (dummy-procedure 'hoge 42 #(1 2 3) 3.14) => 43)

(define length-of-arguments (foreign-function "build/libexample.so" "length_of_arguments"))

(check (foreign-function? length-of-arguments) => #t)
(check (length-of-arguments 'hoge 42 #(1 2 3) 3.14) => 4)

(exit (check-passed? check:correct))
