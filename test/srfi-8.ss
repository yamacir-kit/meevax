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

(emergency-exit (check-passed? check:correct))
