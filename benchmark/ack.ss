(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))

(define m 3)
(define n 7)

(display "Ack(")
(display m)
(display ", ")
(display n)
(display ") = ")
(display (ack m n))
(newline)

(exit)
