(import (scheme base)
        (scheme write)
        (only (srfi 1) circular-list?)
        (srfi 78))

(define x (list 'a 'b 'c))
(set-cdr! (cddr x) x)

(check (circular-list? x) => #t)

(newline)
(write x)
(newline)

(define y (list 'a))
(set-cdr! y y)

(check (circular-list? y) => #t)

(newline)
(write y)
(newline)

(let ((z (list 1)))
  (newline)
  (write (list z z))
  (newline))

(let ((z (list 1)))
  (newline)
  (write-shared (list z z))
  (newline))
