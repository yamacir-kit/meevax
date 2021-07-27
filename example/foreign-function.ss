(define dummy-procedure     (foreign-function "build/libexample-1.so" "dummy_procedure"))
(define length-of-arguments (foreign-function "build/libexample-1.so" "length_of_arguments"))

(check (length-of-arguments 'hoge 42 #(1 2 3) 3.14) => 4)

(exit (check-passed? check:correct))
