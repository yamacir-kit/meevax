(import (scheme base)
        (scheme write)
        (scheme process-context))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

; (exit (= (fib 20) 6765))
(exit (= (fib 30) 832040))
; (exit (= (fib 40) 102334155))
