(import (scheme base)
        (scheme process-context)
        (srfi 78))

(check (values) => '((values)))

(check (values 1) => 1)

(check (values 1 2) => '((values) 1 2))

(define-values a (values 1 2 3))

(define-values (b) (values 1))

(define-values (c d) (values 1 2))

(define-values (e f g) (values 1 2 3))

(define-values (h . i) (values 1 2 3))

(check a => '(1 2 3))

(check b => 1)

(check c => 1)

(check d => 2)

(check e => 1)

(check f => 2)

(check g => 3)

(check h => 1)

(check i => '(2 3))

(let ((j 1)
      (k 2)
      (l 3))
  (define-values (j k l) (values 4 5 6))
  (check j => 4)
  (check k => 5)
  (check l => 6))

(check-report)

(exit (check-passed? 15))
