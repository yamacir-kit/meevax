(import (scheme base)
        (scheme process-context)
        (srfi 78))

; ------------------------------------------------------------------------------

(define-library (test 1)
  (import (scheme base))
  (export f (rename g h))
  (begin (define (f) 'f)
         (define (g) 'g)))

(import (test 1))

(check (f) => f)

(check (h) => g)

; ------------------------------------------------------------------------------

(check-report)

(exit (check-passed? 2))
