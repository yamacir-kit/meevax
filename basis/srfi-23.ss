(define (error message . irritants)
  (display "error: ")
  (display message)
  (for-each (lambda (each)
              (display " ")
              (write each))
            irritants)
  (newline)
  (exit #f))

; (import (srfi 18))
;
; (define (error . xs)
;   (raise (apply make-error xs)))
