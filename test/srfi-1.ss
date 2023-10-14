(import (scheme base)
        (scheme process-context)
        (only (srfi 1)
              xcons
              cons*
              circular-list
              iota
              circular-list?
              dotted-list?
              not-pair?
              null-list?
              list=
              first second third fourth fifth sixth seventh eighth ninth tenth
              take
              last
              last-pair
              length+
              )
        (srfi 78))

(check (cons 'a 'b) => '(a . b))

(check (list 'a 'b 'c) => '(a b c))

(check (xcons 'a 'b) => '(b . a))

(check (cons* 'a) => 'a)
(check (cons* 'a 'b) => '(a . b))
(check (cons* 'a 'b 'c) => '(a b . c))

(check (circular-list 'a) => '#1=(a . #1#))
(check (circular-list 'a 'b) => '#1=(a b . #1#))
(check (circular-list 'a 'b 'c) => '#1=(a b c . #1#))

(check (iota 5) => '(0 1 2 3 4))
(check (iota 5 0 -0.1) (=> (lambda (x y) (list= = x y))) '(0 -0.1 -0.2 -0.3 -0.4))

(check (circular-list? '(a b c)) => #f)
(check (circular-list? '#1=(a b c . #1#)) => #t)

(check (dotted-list? '(a . b)) => #t)
(check (dotted-list? '(a b . c)) => #t)
(check (dotted-list? '(a b c)) => #f)

(check (not-pair? 'a) => #t)
(check (not-pair? '(a . b)) => #f)
(check (not-pair? '(a b . c)) => #f)
(check (not-pair? '(a b c)) => #f)

(check (null-list? '()) => #t)
(check (null-list? '(a b c)) => #f)
(check (null-list? '#1=(a b c . #1#)) => #f)

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

(check (take '(a b c d e) 1) => '(a))
(check (take '(a b c d e) 2) => '(a b))
(check (take '(a b c d e) 3) => '(a b c))
(check (take '(a b c d e) 4) => '(a b c d))
(check (take '(a b c d e) 5) => '(a b c d e))

(check (last '(a)) => 'a)
(check (last '(a b)) => 'b)
(check (last '(a b c)) => 'c)

(check (last-pair '(a)) => '(a))
(check (last-pair '(a b)) => '(b))
(check (last-pair '(a b c)) => '(c))
(check (last-pair '(a b c . d)) => '(c . d))

(check (length+ '(a b c)) => 3)
(check (length+ '(a b . c)) => 2)
(check (length+ '#1=(a b c . #1#)) => #f)

(check-report)

(exit (check-passed? 48))
