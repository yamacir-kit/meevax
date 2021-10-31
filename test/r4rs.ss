(check (* 5 8) => 40)

;;; The FACT procedure computes the factorial
;;; of a non-negative integer.
(define fact
  (lambda (n)
    (if (= n 0)
        1 ; Base case: return 1
        (* n (fact (- n 1))))))


; ---- 4.1. Primitive expression types -----------------------------------------

(define x 28) ; => unspecified

(check x => 28)

(check (quote a) => a)
(check (quote #(a b c)) => #(a b c))
(check (quote (+ 1 2)) => (+ 1 2))

(check 'a => a)
(check '#(a b c) => #(a b c))
(check '() => ())
(check '(+ 1 2) => (+ 1 2))
(check '(quote a) => (quote a))
(check '      'a  => (quote a))

(check '"abc" => "abc")
(check  "abc" => "abc")
(check '145932 => 145932)
(check  145932 => 145932)
(check '#t => #t)
(check  #t => #t)

(check (+ 3 4) => 7)

(check ((if #f + *) 3 4) => 12)

        (lambda (x) (+ x x)) ; => unspecified
(check ((lambda (x) (+ x x)) 4) => 8)

(define reverse-subtract
  (lambda (x y)
    (- y x))) ; => unspecified
(check (reverse-subtract 7 10) => 3)

(define add4
  (let ((x 4))
    (lambda (y) (+ x y))))
(check (add4 6) => 10)

(check ((lambda x x) 3 4 5 6) => (3 4 5 6))
(check
  ((lambda (x y . z) z) 3 4 5 6) => (5 6))

(check (if (> 3 2) 'yes 'no) => yes)
(check (if (> 2 3) 'yes 'no) => no)
(check
  (if (> 3 2)
      (- 3 2)
      (+ 3 2)) => 1)

(define x 2) ; => unspecified
(check (+ x 1) => 3)
(set! x 4) ; => unspecified
(check (+ x 1) => 5)


; ---- 4.2. Derived expression types -------------------------------------------

(check
  (cond ((> 3 2) 'greater)
        ((< 3 2) 'less)) => greater)
(check
  (cond ((> 3 3) 'greater)
        ((< 3 3) 'less)
        (else 'equal)) => equal)

(check
  (cond ((assv 'b '((a 1) (b 2))) => cadr)
        (else #f)) => 2)

(check
  (case (* 2 3)
    ((2 3 5 7) 'prime)
    ((1 4 6 8 9) 'composite)) => composite)
(case (car '(c d))
  ((a) 'a)
  ((b) 'b)) ; => unspecified
(check
  (case (car '(c d))
    ((a e i o u) 'vowel)
    ((w y) 'semivowel)
    (else 'consonant)) => consonant)

(check (and (= 2 2) (> 2 1)) => #t)
(check (and (= 2 2) (< 2 1)) => #f)
(check (and 1 2 'c '(f g)) => (f g))
(check (and) => #t)

(check (or (= 2 2) (> 2 1)) => #t)
(check (or (= 2 2) (< 2 1)) => #t)
(check (or #f #f #f) => #f)
(check
  (or (memq 'b '(a b c))
      (/ 3 0)) => (b c))

(check
  (let ((x 2)
        (y 3))
    (* x y)) => 6)

(check
  (let ((x 2)
        (y 3))
    (let ((x 7)
          (z (+ x y)))
      (* z x))) => 35)

(check
  (let ((x 2)
        (y 3))
    (let* ((x 7)
           (z (+ x y)))
      (* z x))) => 70)

(check
  (letrec ((even?
             (lambda (n)
               (if (zero? n) #t
                   (odd? (- n 1)))))
           (odd?
             (lambda (n)
               (if (zero? n) #f
                   (even? (- n 1))))))
    (even? 88)) => #t)

(define x 0)

(check
  (begin (set! x 5)
         (+ x 1)) => 6)

(begin (display "4 plus 1 equals ")
       (display (+ 4 1))) ; => unspecified

(check
  (do ((vec (make-vector 5))
       (i 0 (+ i 1)))
      ((= i 5) vec)
    (vector-set! vec i i)) => #(0 1 2 3 4))

(check
  (let ((x '(1 3 5 7 9)))
    (do ((x x (cdr x))
         (sum 0 (+ sum (car x))))
        ((null? x) sum))) => 25)

(check
  (let loop ((numbers '(3 -2 1 6 -5))
             (nonneg '())
             (neg '()))
    (cond ((null? numbers)
           (list nonneg neg))
          ((<= 0 (car numbers))
           (loop (cdr numbers)
                 (cons (car numbers) nonneg)
                 neg))
          ((< (car numbers) 0)
           (loop (cdr numbers)
                 nonneg
                 (cons (car numbers) neg))))) => ((6 1 3) (-5 -2)))

(check `(list ,(+ 1 2) 4) => (list 3 4))
(check (let ((name 'a))
         `(list ,name ',name)) => (list a (quote a)))
(check `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b) => (a 3 4 5 6 b))
(check `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))) => ((foo 7) . cons))
(check `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8) => #(10 5 2 4 3 8))

; TODO
; (check
;   `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
;   => (a `(b ,(+ 1 2) ,(foo 4 d) e) f))

; TODO
; (check
;   (let ((name1 'x)
;         (name2 'y))
;     `(a `(b ,,name1 ,',name2 d) e)) => (a `(b ,x ,'y d) e))

(check (quasiquote (list (unquote (+ 1 2)) 4)) => (list 3 4))
(check '(quasiquote (list (unquote (+ 1 2)) 4)) => `(list ,(+ 1 2) 4))


; ---- 5. Program structure ----------------------------------------------------

(define add3
  (lambda (x) (+ x 3))) ; => unspecified
(check (add3 3) => 6)

(define first car) ; => unspecified
(check (first '(1 2)) => 1)

(check
  (let ((x 5))
    (define foo (lambda (y) (bar x y)))
    (define bar (lambda (a b) (+ (* a b) a)))
    (foo (+ x 3))) => 45)

(check
  (let ((x 5))
    (letrec ((foo (lambda (y) (bar x y)))
             (bar (lambda (a b) (+ (* a b) a))))
      (foo (+ x 3)))) => 45)


; ---- 6.1. Booleans -----------------------------------------------------------

(check  #t => #t)
(check '#t => #t)
(check  #f => #f)
(check '#f => #f)

(check (not #t) => #f)
(check (not 3) => #f)
(check (not (list 3)) => #f)
(check (not #f) => #t)
(check (not '()) => #f)
(check (not (list)) => #f)
(check (not 'nil) => #f)

(check (boolean? #f) => #t)
(check (boolean? 0) => #f)
(check (boolean? '()) => #f)


; ---- 6.2. Equivalence predicates ---------------------------------------------

(check (eqv? 'a 'a) => #t)
(check (eqv? 'a 'b) => #f)
(check (eqv? 2 2) => #t)
(check (eqv? '() '()) => #t)
(check (eqv? 100000000 100000000) => #t)
(check (eqv? (cons 1 2) (cons 1 2)) => #f)
(check (eqv? (lambda () 1)
             (lambda () 2)) => #f)
(check (eqv? #f 'nil) => #f)
(check (let ((p (lambda (x) x)))
         (eqv? p p)) => #t)

(check (eqv? "" "") => #t) ; unspecified
(check (eqv? '#() '#()) => #t) ; unspecified
(check (eqv? (lambda (x) x)
             (lambda (x) x)) => #f) ; unspecified
(check (eqv? (lambda (x) x)
             (lambda (y) y)) => #f) ; unspecified

(define generate-counter
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) n)))) ; => unspecified
(check
  (let ((g (generate-counter)))
    (eqv? g g)) => #t)
(check
  (eqv? (generate-counter)
        (generate-counter)) => #f)

(define generate-loser
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) 27)))) ; => unspecified
(check
  (let ((g (generate-loser)))
    (eqv? g g)) => #t)
(check
  (eqv? (generate-loser)
        (generate-loser)) => #f) ; unspecified

(check
  (letrec ((f (lambda () (if (eqv? f g) 'both 'f)))
           (g (lambda () (if (eqv? f g) 'both 'g))))
    (eqv? f g)) => #f) ; unspecified

(check
  (letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
           (g (lambda () (if (eqv? f g) 'g 'both))))
    (eqv? f g)) => #f)

(check (eqv? '(a) '(a)) => #t) ; unspecified
(check (eqv? "a" "a") => #t) ; unspecified
(check (eqv? '(b) (cdr '(a b))) => #t) ; unspecified
(check (let ((x '(a)))
         (eqv? x x)) => #t)

(check (eq? 'a 'a) => #t)
(check (eq? '(a) '(a)) => #f) ; unspecified
(check (eq? (list 'a) (list 'a)) => #f)
(check (eq? "a" "a") => #f) ; unspecified
(check (eq? "" "") => #f) ; unspecified
(check (eq? '() '()) => #t)
(check (eq? 2 2) => #f) ; unspecified
(check (eq? #\A #\A) => #f) ; unspecified
(check (eq? car car) => #t)
(check (let ((n (+ 2 3)))
         (eq? n n)) => #t) ; unspecified
(check (let ((x '(a)))
         (eq? x x)) => #t)
(check (let ((x '#()))
         (eq? x x)) => #t)
(check (let ((p (lambda (x) x)))
         (eq? p p)) => #t)

(check (equal? 'a 'a) => #t)
(check (equal? '(a) '(a)) => #t)
(check (equal? '(a (b) c)
               '(a (b) c)) => #t)
(check (equal? "abc" "abc") => #t)
(check (equal? 2 2) => #t)
(check (equal? (make-vector 5 'a)
               (make-vector 5 'a)) => #t)
(check (equal? (lambda (x) x)
               (lambda (y) y)) => #f) ; unspecified


; ---- 6.3. Pairs and lists ----------------------------------------------------

(check
  (equal? '(a b c d e)
          '(a . (b . (c . (d . (e . ())))))) => #t)

(check
  (equal? '(a b c . d)
          '(a . (b . (c . d)))) => #t)

(define x (list 'a 'b 'c))
(define y x)
(check x => (a b c))
(check y => (a b c))
(check (list? x) => #t)
(check (list? y) => #t)
(set-cdr! x 4) ; => unspecified
(check x => (a . 4))
(check y => (a . 4))
(check (eqv? x y) => #t)
(check (list? x) => #f)
(check (list? y) => #f)
(set-cdr! x x) ; => unspecified
(check (list? x) => #f)

(check (pair? '(a . b)) => #t)
(check (pair? '(a b c)) => #t)
(check (pair? '()) => #f)
(check (pair? '#(a b)) => #f)

(check (cons 'a '()) => (a))
(check (cons '(a) '(b c d)) => ((a) b c d))
(check (cons "a" '(b c)) => ("a" b c))
(check (cons 'a 3) => (a . 3))
(check (cons '(a b) 'c) => ((a b) . c))

(check (car '(a b c)) => a)
(check (car '((a) b c d)) => (a))
(check (car '(1 . 2)) => 1)
; (check (car '()) => error)

(check (cdr '((a) b c d)) => (b c d))
(check (cdr '(1 . 2)) => 2)
; (check (cdr '()) => error)

(define (f) (list 'not-a-constant-list))
(define (g) '(constant-list))
(set-car! (f) 3) ; => unspecified
(set-car! (g) 3) ; => error

(check (null? ()) => #t)
(check (null? '()) => #t)
(check (null? '(a)) => #f)
(check (null? '(a b c)) => #f)
(check (null? (list)) => #t)
(check (null? (list 'a)) => #f)
(check (null? (list 'a 'b 'c)) => #f)
(check (null? 42) => #f)

(check (list? '(a b c)) => #t)
(check (list? '()) => #t)
(check (list? '(a)) => #t)
(check (list? '(a . b)) => #f)
(check
  (let ((x (list 'a)))
    (set-cdr! x x)
    (list? x)) => #f)

(check (list 'a (+ 3 4) 'c) => (a 7 c))
(check (list) => ())

(check (length '(a b c)) => 3)
(check (length '(a (b) (c d e))) => 3)
(check (length '()) => 0)

(check (append '(x) '(y)) => (x y))
(check (append '(a) '(b c d)) => (a b c d))
(check (append '(a (b)) '((c))) => (a (b) (c)))
(check (append '(a b) '(c . d)) => (a b c . d))
(check (append '() 'a) => a)

(check (reverse '(a b c)) => (c b a))
(check (reverse '(a (b c) d (e (f)))) =>  ((e (f)) d (b c) a))

(check (list-ref '(a b c d) 2) => c)
(check
  (list-ref '(a b c d)
             (exact (round 1.8))) => c)

(check (memq 'a '(a b c)) => (a b c))
(check (memq 'b '(a b c)) => (b c))
(check (memq 'a '(b c d)) => #f)
(check (memq (list 'a) '(b (a) c)) => #f)
(check (member (list 'a) '(b (a) c)) => ((a) c))
(check (memq 101 '(100 101 102)) => #f) ; unspecified
(check (memv 101 '(100 101 102)) => (101 102))

(define e '((a 1) (b 2) (c 3)))
(check (assq 'a e) => (a 1))
(check (assq 'b e) => (b 2))
(check (assq 'c e) => (c 3))
(check (assq 'd e) => #f)
(check (assq (list 'a) '(((a)) ((b)) ((c)))) => #f)
(check (assoc (list 'a) '(((a)) ((b)) ((c)))) => ((a)))
(check (assq 5 '((2 3) (5 7) (11 13))) => #f) ; unspecified
(check (assv 5 '((2 3) (5 7) (11 13))) => (5 7))


; ---- 6.4. Symbols ------------------------------------------------------------

(check (symbol? 'foo) => #t)
(check (symbol? (car '(a b))) => #t)
(check (symbol? "bar") => #f)
(check (symbol? 'nil) => #t)
(check (symbol? '()) => #f)
(check (symbol? #f) => #f)

(check (symbol->string 'flying-fish) => "flying-fish")
(check (symbol->string 'Martin) => "Martin")
(check (symbol->string
         (string->symbol "Malvina")) => "Malvina")

(check (eq? 'mISSISSIppi 'mississippi) => #f) ; R7RS
(check (string->symbol "mISSISSIppi") => mISSISSIppi)
; (check (eq? 'bitBlt (string->symbol "bitBlt")) => #f)
(check (eqv? 'JollyWog ; in R7RS, eq? level equality is not required.
             (string->symbol
               (symbol->string 'JollyWog))) => #t)
(check (string=? "K. Harper, M.D."
                 (symbol->string
                   (string->symbol "K. Harper, M.D."))) => #t)


; ---- 6.5. Numbers ------------------------------------------------------------

; (check (complex? 3+4i) => #t)
(check (complex? 3) => #t)
(check (real? 3) => #t)
; (check (real? -2.5+0.0i) => #t)
; (check (real? #e1e10) => #t) ; TODO floating_point.hpp's rationalize overflows if large number given.
(check (rational? 6/10) => #t)
(check (rational? 6/3) => #t)
; (check (integer? 3+0i) => #t)
(check (integer? 3.0) => #t)
(check (integer? 8/4) => #t)

(check (max 3   4) => 4) ; exact
(check (max 3.9 4) => 4.0) ; inexact

(check (+ 3 4) => 7)
(check (+ 3) => 3)
(check (+) => 0)
(check (* 4) => 4)
(check (*) => 1)

(check (- 3) => -3)
(check (- 3 4) => -1)
(check (- 3 4 5) => -6)
(check (/ 3) => 1/3)
(check (/ 3 4 5) => 3/20)

(check (abs -7) => 7)

(check (floor -4.3) => -5.0)
(check (ceiling -4.3) => -4.0)
(check (truncate -4.3) => -4.0)
(check (round -4.3) => -4.0)

(check (floor 3.5) => 3.0)
(check (ceiling 3.5) => 4.0)
(check (truncate 3.5) => 3.0)
(check (round 3.5) => 4.0) ; inexact

(check (round 7/2) => 4) ; exact
(check (round 7) => 7)

(check (rationalize
         (inexact->exact .3) 1/10) => 1/3) ; exact
(check (rationalize .3  1/10) => #,(/ 1.0 3.0)) ; inexact

(check (string->number "100") => 100)
(check (string->number "100" 16) => 256)
(check (string->number "1e2") => 100.0)
; (check (string->number "15##") => 1500.0) ; R7RS removed


; ---- 6.6. Characters ---------------------------------------------------------

(check (char? #\alarm)     => #t) ; U+0007
(check (char? #\backspace) => #t) ; U+0008
(check (char? #\delete)    => #t) ; U+007F
(check (char? #\escape)    => #t) ; U+001B
(check (char? #\newline)   => #t) ; U+000A
(check (char? #\null)      => #t) ; U+0000
(check (char? #\return)    => #t) ; U+000D
(check (char? #\space)     => #t) ;
(check (char? #\tab)       => #t) ; U+0009

(check (char? #\a) => #t)
(check (char? #\A) => #t)
(check (char? #\() => #t)
(check (char? #\ ) => #t)

(check (char<? #\A #\B) => #t)
(check (char<? #\a #\b) => #t)
(check (char<? #\0 #\9) => #t)

(check (char-ci=? #\A #\a) => #t)


; ---- 6.7. Strings ------------------------------------------------------------

(check (string? "The word \"recursion\" has many meanings.") => #t)

(define (f) (make-string 3 #\*))
(define (g) "***")

(check (string-set! (f) 0 #\?) => "?**")
(check (string-set! (g) 0 #\?) => "?**")
(check (string-set! (symbol->string 'immutable) 0 #\?) => "?mmutable")


; ---- 6.8. Vectors ------------------------------------------------------------

(check (vector? #(0 (2 2 2 2) "Anna")) => #t)
(check '#(0 (2 2 2 2) "Anna") => #(0 (2 2 2 2) "Anna"))

(check (vector 'a 'b 'c) => #(a b c))

(check (vector-ref '#(1 1 2 3 5 8 13 21) 5) => 8)
(check (vector-ref '#(1 1 2 3 5 8 13 21)
                   (inexact->exact
                     (round (* 2 (acos -1))))) => 13)

(check (let ((vec (vector 0 '(2 2 2 2) "Anna")))
         (vector-set! vec 1 '("Sue" "Sue"))
         vec) => #(0 ("Sue" "Sue") "Anna"))
(check (vector-set! '#(0 1 2) 1 "doe") => "doe")

(check (vector->list '#(dah dah didah)) => (dah dah didah))
(check (list->vector '(dididit dah)) => #(dididit dah))


; ---- 6.9. Control features ---------------------------------------------------

(check (procedure? car) => #t)
(check (procedure? 'car) => #f)
(check (procedure? (lambda (x) (* x x))) => #t)
(check (procedure? '(lambda (x) (* x x))) => #f)
(check (call-with-current-continuation procedure?) => #t)

(check (apply + (list 3 4)) => 7)

(define compose
  (lambda (f g)
    (lambda args
      (f (apply g args)))))

(check ((compose sqrt *) 12 75) => 30)

(check (map cadr '((a b) (d e) (g h))) => (b e h))
(check (map (lambda (n) (expt n n))
            '(1 2 3 4 5)) => (1 4 27 256 3125))
(check (map + '(1 2 3) '(4 5 6 7)) => (5 7 9))

(check (let ((count 0))
         (map (lambda (ignored)
                (set! count (+ count 1))
                count)
              '(a b c))) => (1 2 3))

(check (let ((v (make-vector 5)))
         (for-each (lambda (i)
                     (vector-set! v i (* i i)))
                   '(0 1 2 3 4))
         v) => #(0 1 4 9 16))

(check (force (delay (+ 1 2))) => 3)
(check (let ((p (delay (+ 1 2))))
         (list (force p)
               (force p))) => (3 3))
(define a-stream
  (letrec ((next
             (lambda (n)
               (cons n (delay (next (+ n 1)))))))
    (next 0)))
(define head car)
(define tail (lambda (stream) (force (cdr stream))))
(check (head (tail (tail a-stream))) => 2)

(define count 0)
(define p
  (delay (begin (set! count (+ count 1))
                (if (< x count)
                    count
                    (force p)))))

(define x 5)
(check (promise? p) => #t)
(check (force p) => 6)
(check (promise? p) => #t)
(check (begin (set! x 10)
              (force p)) => 6)

(check (call-with-current-continuation
         (lambda (exit)
           (for-each (lambda (x)
                       (if (negative? x)
                           (exit x)))
                     '(54 0 37 -3 245 19))
           #t)) => -3)

(define list-length
  (lambda (object)
    (call-with-current-continuation
      (lambda (return)
        (letrec ((r
                   (lambda (object)
                     (cond ((null? object) 0)
                           ((pair? object)
                            (+ (r (cdr object)) 1))
                           (else (return #f))))))
          (r object))))))

(check (list-length '(1 2 3 4)) => 4)
(check (list-length '(a b . c)) => #f)


; ---- 6.10. Input and output --------------------------------------------------


; ------------------------------------------------------------------------------

(check-report)

(exit (check-passed? check:correct))
