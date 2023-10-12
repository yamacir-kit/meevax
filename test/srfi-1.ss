(import (scheme base)
        (scheme process-context)
        (only (srfi 1)
              xcons
              circular-list
              circular-list?
              last
              last-pair
              )
        (srfi 78))

(check (cons 'a 'b) => '(a . b))

(check (list 1 2 3) => '(1 2 3))

(check (xcons 'a 'b) => '(b . a))

(check (circular-list 1) => '#1=(1 . #1#))
(check (circular-list 1 2) => '#1=(1 2 . #1#))
(check (circular-list 1 2 3) => '#1=(1 2 3 . #1#))

(check (circular-list? '(1 2 3)) => #f)
(check (circular-list? '#1=(1 2 3 . #1#)) => #t)

(check (last '(a)) => 'a)
(check (last '(a b)) => 'b)
(check (last '(a b c)) => 'c)

(check (last-pair '(a)) => '(a))
(check (last-pair '(a b)) => '(b))
(check (last-pair '(a b c)) => '(c))
(check (last-pair '(a b c . d)) => '(c . d))

(check-report)

(exit (check-passed? 15))
