(define-library (hoge)
  (import (scheme base))
  (export f (rename g h))
  (begin (define (f) 'f)
         (define (g) 'g)))

(import (scheme base)
        (scheme process-context)
        (srfi 78)
        (hoge))

(check (f) => f)

(check (h) => g)

(check-report)

(exit (check-passed? 2))
