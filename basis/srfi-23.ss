; (import (srfi 18))

(define (error . xs)
  (raise (apply make-error xs)))
