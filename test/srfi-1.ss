(import (scheme base)
        (scheme process-context)
        (only (srfi 1)
              xcons
              cons*
              list-tabulate
              circular-list
              iota
              circular-list?
              dotted-list?
              not-pair?
              null-list?
              list=
              first second third fourth fifth sixth seventh eighth ninth tenth
              take take!
              drop
              take-right
              drop-right drop-right!
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

(check (list-tabulate 4 (lambda (x) x)) => '(0 1 2 3))
(check (list-tabulate 4 number->string) => '("0" "1" "2" "3"))

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

(check (take '(a b c d e) 0) => '())
(check (take '(a b c d e) 1) => '(a))
(check (take '(a b c d e) 2) => '(a b))
(check (take '(a b c d e) 3) => '(a b c))
(check (take '(a b c d e) 4) => '(a b c d))
(check (take '(a b c d e) 5) => '(a b c d e))

(check (drop '(a b c d e) 0) => '(a b c d e))
(check (drop '(a b c d e) 1) => '(b c d e))
(check (drop '(a b c d e) 2) => '(c d e))
(check (drop '(a b c d e) 3) => '(d e))
(check (drop '(a b c d e) 4) => '(e))
(check (drop '(a b c d e) 5) => '())

(check (take-right '(a b c d e) 0) => '())
(check (take-right '(a b c d e) 1) => '(e))
(check (take-right '(a b c d e) 2) => '(d e))
(check (take-right '(a b c d e) 3) => '(c d e))
(check (take-right '(a b c d e) 4) => '(b c d e))
(check (take-right '(a b c d e) 5) => '(a b c d e))
(check (take-right '(a b c . x) 0) => 'x)
(check (take-right '(a b c . x) 1) => '(c . x))
(check (take-right '(a b c . x) 2) => '(b c . x))
(check (take-right '(a b c . x) 3) => '(a b c . x))

(check (drop-right '(a b c d e) 0) => '(a b c d e))
(check (drop-right '(a b c d e) 1) => '(a b c d))
(check (drop-right '(a b c d e) 2) => '(a b c))
(check (drop-right '(a b c d e) 3) => '(a b))
(check (drop-right '(a b c d e) 4) => '(a))
(check (drop-right '(a b c d e) 5) => '())
(check (drop-right '(a b c . x) 0) => '(a b c))
(check (drop-right '(a b c . x) 1) => '(a b))
(check (drop-right '(a b c . x) 2) => '(a))
(check (drop-right '(a b c . x) 3) => '())

(let ((x '(a b c d e))) (check (take! x 0) => '())          (check x => '(a b c d e)))
(let ((x '(a b c d e))) (check (take! x 1) => '(a))         (check x => '(a)))
(let ((x '(a b c d e))) (check (take! x 2) => '(a b))       (check x => '(a b)))
(let ((x '(a b c d e))) (check (take! x 3) => '(a b c))     (check x => '(a b c)))
(let ((x '(a b c d e))) (check (take! x 4) => '(a b c d))   (check x => '(a b c d)))
(let ((x '(a b c d e))) (check (take! x 5) => '(a b c d e)) (check x => '(a b c d e)))

(let ((x '(a b c d e))) (check (drop-right! x 0) => '(a b c d e)) (check x => '(a b c d e)))
(let ((x '(a b c d e))) (check (drop-right! x 1) => '(a b c d))   (check x => '(a b c d)))
(let ((x '(a b c d e))) (check (drop-right! x 2) => '(a b c))     (check x => '(a b c)))
(let ((x '(a b c d e))) (check (drop-right! x 3) => '(a b))       (check x => '(a b)))
(let ((x '(a b c d e))) (check (drop-right! x 4) => '(a))         (check x => '(a)))
(let ((x '(a b c d e))) (check (drop-right! x 5) => '())          (check x => '(a b c d e)))
(let ((x '(a b c . z))) (check (drop-right! x 0) => '(a b c))     (check x => '(a b c)))
(let ((x '(a b c . z))) (check (drop-right! x 1) => '(a b))       (check x => '(a b)))
(let ((x '(a b c . z))) (check (drop-right! x 2) => '(a))         (check x => '(a)))
(let ((x '(a b c . z))) (check (drop-right! x 3) => '())          (check x => '(a b c . z)))

(check (last '(a)) => 'a)
(check (last '(a b)) => 'b)
(check (last '(a b c)) => 'c)

(check (last-pair '(a)) => '(a))
(check (last-pair '(a b)) => '(b))
(check (last-pair '(a b c)) => '(c))
(check (last-pair '(a b c . d)) => '(c . d))

(check (length '(a b c)) => 3)
(check (length '(a b . c)) => 2)

(check (length+ '(a b c)) => 3)
(check (length+ '(a b . c)) => 2)
(check (length+ '#1=(a b c . #1#)) => #f)

(check-report)

(exit (check-passed? 111))
