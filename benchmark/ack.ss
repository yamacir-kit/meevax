(import (scheme base)
        (scheme write)
        (scheme process-context))

(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))

(display (ack 3 8))
(newline)

(exit)
