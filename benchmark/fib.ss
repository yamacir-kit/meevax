(import (scheme base)
        (scheme write)
        (scheme process-context))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

; (fib 20) = 6765
; (fib 30) = 832040
; (fib 40) = 102334155

(exit (= (fib 30) 832040))
