(check-set-mode! 'report)

; ==== 4.1. Primitive expression types =========================================

; ---- 4.1.1. Variable references ----------------------------------------------

(define x 28) ; => unspecified

(check x => 28)

; ---- 4.1.2. Literal expressions ----------------------------------------------

(check (quote a) => a)
; (check (quote #(a b c)) => #(a b c))
(check (quote (+ 1 2)) => (+ 1 2))

(check 'a => a)
; (check '#(a b c) => #(a b c))
(check '(+ 1 2) => (+ 1 2))

(check '() => ())

(check '(quote a) => (quote a))
(check       ''a  => (quote a))

(check '"abc" => "abc")
(check  "abc" => "abc")

(check '145932 => 145932)
(check  145932 => 145932)

(check '#t => #t)
(check  #t => #t)

; ---- 4.1.3. Procedure calls --------------------------------------------------

(check (+ 3 4) => 7)

(check ((if #f + *) 3 4) => 12)

; ---- 4.1.4. Lambda expressions -----------------------------------------------

(lambda (x) (+ x x)) ; => unspecified

(check ((lambda (x) (+ x x)) 4) => 8)

(define reverse-subtract
  (lambda (x y)
    (- y x))) ; => unspecified

(check (reverse-subtract 7 10) => 3)

(define add4
  (let ((x 4))
    (lambda (y)
      (+ x y))))

(check (add4 6) => 10)

(check ((lambda x x) 3 4 5 6) => (3 4 5 6))

(check
  ((lambda (x y . z) z) 3 4 5 6)
  => (5 6))

; ---- 4.1.5. Conditionals -----------------------------------------------------

(check (if (> 3 2) 'yes 'no) => yes)
(check (if (> 2 3) 'yes 'no) => no)

(check
  (if (> 3 2)
      (- 3 2)
      (+ 3 2))
  => 1)

; ---- 4.1.6. Assignments ------------------------------------------------------

(define x 2) ; => unspecified

(check (+ x 1) => 3)

(set! x 4) ; => unspecified

(check (+ x 1) => 5)


; ==== 4.2. Derived expression types ===========================================

; ---- 4.2.1. Conditionals -----------------------------------------------------

(check
  (cond ((> 3 2) 'greater)
        ((< 3 2) 'less))
  => greater)

(check
  (cond ((> 3 3) 'greater)
        ((< 3 3) 'less)
        (else 'equal))
  => equal)

(check
  (cond ((assv 'b '((a 1) (b 2))) => cadr)
        (else #f))
  => 2)

(check
  (case (* 2 3)
    ((2 3 5 7) 'prime)
    ((1 4 6 8 9) 'composite))
  => composite)

(case (car '(c d))
  ((a) 'a)
  ((b) 'b)) ; => unspecified

(check
  (case (car '(c d))
    ((a e i o u) 'vowel)
    ((w y) 'semivowel)
    (else 'consonant))
  => consonant)

(check
  (and (= 2 2)
       (> 2 1))
  => #t)

(check
  (and (= 2 2)
       (< 2 1))
  => #f)

(check
  (and 1 2 'c '(f g))
  => (f g))

(check (and) => #t)

(check
  (or (= 2 2)
      (> 2 1))
  => #t)

(check
  (or (= 2 2)
      (< 2 1))
  => #t)

(check (or #f #f #f) => #f)

(check
  (or (memq 'b '(a b c))
      (/ 3 0))
  => (b c))

; ---- 4.2.2. Binding constructs -----------------------------------------------

(check
  (let ((x 2)
        (y 3))
    (* x y))
  => 6)

(check
  (let ((x 2)
        (y 3))
    (let ((x 7)
          (z (+ x y)))
      (* z x)))
  => 35)

(check
  (let ((x 2)
        (y 3))
    (let* ((x 7)
           (z (+ x y)))
      (* z x)))
  => 70)

(check
  (letrec ((even?
             (lambda (n)
               (if (zero? n) #t
                   (odd? (- n 1)))))
           (odd?
             (lambda (n)
               (if (zero? n) #f
                   (even? (- n 1))))))
    (even? 88))
  => #t)

; ---- 4.2.3. Sequencing -------------------------------------------------------

(define x 0)

(check
  (begin (set! x 5)
         (+ x 1))
  => 6)

(begin (display "4 plus 1 equals ")
       (display (+ 4 1))) ; => unspecified

; ---- 4.2.4. Iteration --------------------------------------------------------

; (check
;   (do ((vec (make-vector 5))
;        (i 0 (+ i 1)))
;       ((= i 5) vec)
;     (vector-set! vec i i))
;   => #(0 1 2 3 4))

(check
  (let ((x '(1 3 5 7 9)))
    (do ((x x (cdr x))
         (sum 0 (+ sum (car x))))
        ((null? x) sum)))
  => 25)

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
                 (cons (car numbers) neg)))))
  => ((6 1 3) (-5 -2)))

; ---- 4.2.5. Delayed expression -----------------------------------------------

; No example.

; ---- 4.2.6. Quasiquote -------------------------------------------------------

(check
  `(list ,(+ 1 2) 4)
  => (list 3 4))

(check
  (let ((name 'a))
    `(list ,name ',name))
  => (list a (quote a)))

(check
  `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)
  => (a 3 4 5 6 b))

(check
  `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))
  => ((foo 7) . cons))

; (check
;  `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)
;   => #(10 5 2 4 3 8))

; NOTE: Fails due to syntactic-continuation's external representation.
; (check
;   `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
;   => (a `(b ,(+ 1 2) ,(foo 4 d) e) f))

; NOTE: Fails due to syntactic-continuation's external representation.
; (check
;   (let ((name1 'x)
;         (name2 'y))
;    `(a `(b ,,name1 ,',name2 d) e))
;   => (a `(b ,x ,'y d) e))

(check
  (quasiquote (list (unquote (+ 1 2)) 4))
  => (list 3 4))

(check
  '(quasiquote (list (unquote (+ 1 2)) 4))
  => `(list ,(+ 1 2) 4))


; ==== 5.1. Programs ===========================================================

; No example.


; ==== 5.2. Definitions ========================================================

; ---- 5.2.1. Top level definitions --------------------------------------------

(define add3
  (lambda (x)
    (+ x 3))) ; => unspecified

(check (add3 3) => 6)

(define first car) ; => unspecified

(check (first '(1 2)) => 1)

; ---- 5.2.2. Internal definitions ---------------------------------------------

(check
  (let ((x 5))
    (define foo
      (lambda (y)
        (bar x y)))
    (define bar
      (lambda (a b)
        (+ (* a b) a)))
    (foo (+ x 3)))
  => 45)

(check
  (let ((x 5))
    (letrec* ((foo
                (lambda (y)
                  (bar x y)))
              (bar
                (lambda (a b)
                  (+ (* a b) a))))
      (foo (+ x 3))))
  => 45)


; ==== 6. Standard procedures ==================================================

; ---- 6.1. Booleans -----------------------------------------------------------

(check  #t => #t)
(check '#t => #t)
(check  #f => #f)
(check '#f => #f)

(check (not #t) => #f)
(check (not 3) => #f)
(check (not (list 3)) => #f)
(check (not #f) => #t)
; (check (not '()) => #f) ; SEGV
; (check (not (list)) => #f) ; SEGV
(check (not 'nil) => #f)

(check (boolean? #f) => #t)
(check (boolean? 0) => #f)
(check (boolean? '()) => #f)

; ---- 6.2. Equivalence predicates ---------------------------------------------

(check (eqv? 'a 'a) => #t)
(check (eqv? 'a 'b) => #f)

(check (eqv? 2 2) => #t)

(check (eqv? '() '()) => #t)

(check
  (eqv? 100000000
        100000000)
  => #t)

(check
  (eqv? (cons 1 2)
        (cons 1 2))
  => #f)

(check
  (eqv? (lambda () 1)
        (lambda () 2))
  => #f)

(check (eqv? #f 'nil) => #f)

(check
  (let ((p (lambda (x) x)))
    (eqv? p p))
  => #t)

(check
  (eqv? "" "") ; => unspecified
  => #t)

(check
  (eqv? "abc" "abc") ; => unspecified
  => #t)

; (check (eqv? '#() '#() => (unspecified))

(check
  (eqv? (lambda (x) x)
        (lambda (x) x)) ; => unspecified
  => #f)

(check
  (eqv? (lambda (x) x)
        (lambda (x) y)) ; => unspecified
  => #f)

(define generate-counter
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) n)))) ; => unspecified

(check
  (let ((g (generate-counter)))
    (eqv? g g))
  => #t)

(check
  (eqv? (generate-counter)
        (generate-counter))
  => #f)

(define generate-loser
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) 27)))) ; => unspecified

(check
  (let ((g (generate-loser)))
    (eqv? g g))
  => #t)

(check
  (eqv? (generate-loser)
        (generate-loser)) ; => unspecified
  => #f)

(check
  (letrec ((f (lambda () (if (eqv? f g) 'both 'f)))
           (g (lambda () (if (eqv? f g) 'both 'g))))
    (eqv? f g)) ; => unspecified
  => #f)

(check
  (letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
           (g (lambda () (if (eqv? f g) 'g 'both))))
    (eqv? f g)) ; => unspecified
  => #f)

(check (eqv? '(a) '(a)) => #t) ; unspecified
(check (eqv? "a" "a") => #t) ; unspecified
(check (eqv? '(b) (cdr '(a b))) => #t) ; unspecified

(check
  (let ((x '(a)))
    (eqv? x x))
  => #t)

(check (eq? 'a 'a) => #t)
(check (eq? '(a) '(a)) => #f) ; unspecified

(check
  (eq? (list 'a)
       (list 'a))
  => #f)

(check (eq? "a" "a") => #f) ; unspecified
(check (eq? "" "") => #t) ; unspecified

(check (eq? '() '()) => #t)

(check (eq? 2 2) => #f) ; unspecified
(check (eq? #\A #\A) => #t) ; unspecified

(check (eq? car car) => #t)

(check
  (let ((n (+ 2 3)))
    (eq? n n)) ; unspecified
  => #t)

(check
  (let ((x '(a)))
    (eq? x x))
  => #t)

; (check
;   (let ((x '#()))
;     (eq? x x))
;   => #t)

(check
  (let ((p (lambda (x) x)))
    (eq? p p))
  => #t)

(check (equal? 'a 'a) => #t)
(check (equal? '(a) '(a)) => #t)

(check
  (equal? '(a (b) c)
          '(a (b) c))
  => #t)

(check
  (equal? "abc"
          "abc")
  => #t)

(check (equal? 2 2) => #t)

; (check
;   (equal? (make-vector 5 'a)
;           (make-vector 5 'a))
;   => #t)

(check
  (equal? (lambda (x) x)
          (lambda (y) y)) ; unspecified
  => #f)


; ==== REPORT ==================================================================

(check-report)

(exit (if (check-passed? check::correct)
          exit-success
          exit-failure))
