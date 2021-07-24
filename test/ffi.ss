(define dummy-procedure
  (foreign-function  "libmeevax-test-foreign-function-interface.so" "dummy_procedure"))

(check (dummy-procedure "hello, world!\n" 42 '(1 . 2) #(1 2 3) 3.14) => 43)
