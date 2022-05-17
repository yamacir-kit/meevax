(import (scheme base)
        (scheme process-context)
        (srfi 8)
        (srfi 78))

(check
  (call-with-values (lambda () (values 4 5))
                    (lambda (a b) b))
  => 5)

(check
  (receive (a b) (values 4 5) b) => 5)

(check
  (receive (q r) (truncate/ 13 4)
    (list q r))
  => (3 1))

(check
  (receive q/r (truncate/ 13 4)
    q/r)
  => (3 1))

(check
  (receive (q . rest) (truncate/ 13 4)
    (list q rest))
  => (3 (1)))

(check-report)

(exit (check-passed? 5))
