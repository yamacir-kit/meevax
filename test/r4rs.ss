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


; ==== REPORT ==================================================================

(check-report)

(exit (if (check-passed? check::correct)
          exit-success
          exit-failure))
