(import (scheme base)
        (scheme process-context)
        (only (srfi 1)
              xcons
              circular-list
              circular-list?
              first second third fourth fifth sixth seventh eighth ninth tenth
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

(check (first   '(a b c d e f g h i j)) => 'a)
(check (second  '(a b c d e f g h i j)) => 'b)
(check (third   '(a b c d e f g h i j)) => 'c)
(check (fourth  '(a b c d e f g h i j)) => 'd)
(check (fifth   '(a b c d e f g h i j)) => 'e)
(check (sixth   '(a b c d e f g h i j)) => 'f)
(check (seventh '(a b c d e f g h i j)) => 'g)
(check (eighth  '(a b c d e f g h i j)) => 'h)
(check (ninth   '(a b c d e f g h i j)) => 'i)
(check (tenth   '(a b c d e f g h i j)) => 'j)

(check (last '(a)) => 'a)
(check (last '(a b)) => 'b)
(check (last '(a b c)) => 'c)

(check (last-pair '(a)) => '(a))
(check (last-pair '(a b)) => '(b))
(check (last-pair '(a b c)) => '(c))
(check (last-pair '(a b c . d)) => '(c . d))

(check-report)

(exit (check-passed? 25))