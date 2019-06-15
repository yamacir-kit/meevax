(load "../setup.scm")

(define test-passed 0)

(define test
  (macro (expression expects)
   `(let ((result ,expression))
      (if (equal? result ',expects)
          (begin (set! test-passed (+ test-passed 1))
                 result)
          (begin (display "; test          ; expected ") ; TODO SUPPORT TAB
                 (display ',expects)
                 (display " as result of ")
                 (display ',expression)
                 (display ", but got ")
                 (display result)
                 (display ".")
                 (newline)
                 (emergency-exit))))))


; ------------------------------------------------------------------------------
;   4.1.1 Variable References
; ------------------------------------------------------------------------------

(define x 28)

(test x 28)


; ------------------------------------------------------------------------------
;   4.1.2 Literal Expressions
; ------------------------------------------------------------------------------

(test
  (quote a)
  a)

; (test
;   (quote #(a b c)) ; unimplemented
;   #(a b c))

(test
  (quote (+ 1 2))
  (+ 1 2))

(test 'a a)

; (test
;  '#(a b c)
;   #(a b c)) ; unimplemented

(test
 '()
  ())

(test
 '(+ 1 2)
  (+ 1 2))

(test
 '(quote a)
  (quote a))

(test
  ''a
  (quote a))

(test '145932 145932)
(test  145932 145932)

; (test '"abc" "abc")
; (test  "abc" "abc") ; マクロ中で文字列の挙動がおかしいバグあり

; (test '# #)
; (test  # #) ; unimplemented

; (test '#(a 10) #(a 10))
; (test  #(a 10) #(a 10)) ; unimplemented

; (test '#u8(64 65) #u8(64 65))
; (test  #u8(64 65) #u8(64 65)) ; unimplemented

(test '#t #t)
(test  #t #t)


; ------------------------------------------------------------------------------
;   4.1.3 Procedure Calls
; ------------------------------------------------------------------------------

(test (+ 3 4) 7)

(test
  ((if #false + *) 3 4)
  12)


; ------------------------------------------------------------------------------
;   4.1.4 Procedures
; ------------------------------------------------------------------------------

; (lambda (x) (+ x x)) ; untestable output

(test
  ((lambda (x) (+ x x)) 4)
  8)

(define reverse-subtract
  (lambda (x y)
    (- y x)))

(test
  (reverse-subtract 7 10)
  3)

(define add4
  (let ((x 4))
    (lambda (y) (+ x y))))

(test
  (add4 6)
  10)

(test
  ((lambda x x) 3 4 5 6)
  (3 4 5 6))

(test
  ((lambda (x y . z) z) 3 4 5 6)
  (5 6))


; ------------------------------------------------------------------------------
;   4.1.5 Conditionals
; ------------------------------------------------------------------------------

(test
  (if (> 3 2) 'yes 'no)
  yes)

(test
  (if (> 2 3) 'yes 'no)
  no)

(test
  (if (> 3 2)
      (- 3 2)
      (+ 3 2))
  1)


; ------------------------------------------------------------------------------
;   4.1.6 Assignments
; ------------------------------------------------------------------------------

(define x 2)

(test
  (+ x 1)
  3)

(set! x 4) ; #undefined

(test
  (+ x 1)
  5)


; ------------------------------------------------------------------------------
;   4.2.1 Conditionals
; ------------------------------------------------------------------------------

(test
  (cond ((> 3 2) 'greater)
        ((< 3 2) 'less))
  greater)

(test
  (cond ((> 3 3) 'greater)
        ((< 3 3) 'less)
        (else 'equal))
  equal)

; (test
;   (cond ((assv 'b '((a 1) (b 2))) => cadr)
;         (else #f))
;   2)

(test
  (case (* 2 3)
    ((2 3 5 7) 'prime)
    ((1 4 6 8 9) 'composite))
  composite)

; (test
;   (case (car '(c d))
;     ((a) 'a)
;     ((b) 'b))
;   undefined)

; (test
;   (case (car '(c d))
;     ((a e i o u) 'vowel)
;     ((w y) 'semivowel)
;     (else => (lambda (x) x)))
;   c)

(test
  (and (= 2 2)
       (> 2 1))
  #true)

(test
  (and (= 2 2)
       (< 2 1))
  #false)

(test
  (and 1 2 'c '(f g))
  (f g))

(test (and) #true)

(test
  (or (= 2 2)
      (> 2 1))
  #true)

(test
  (or (= 2 2)
      (< 2 1))
  #true)

(test
  (or #false #false #false)
  #false)

; (test
;   (or (memq 'b '(a b c)) ; memq unimplemented
;       (/ 3 0))
;   (b c))

; (when (= 1 1.0)
;   (display "1")
;   (display "2")); => #unspecified and prints 12

; (unless (= 1 1.0)
;   (display "1")
;   (display "2")); => #unspecified and prints nothing


; ------------------------------------------------------------------------------
;   4.2.2 Binding Constructs
; ------------------------------------------------------------------------------

(test
  (let ((x 2)
        (y 3))
    (* x y))
  6)

(test
  (let ((x 2)
        (y 3))
    (let ((x 7)
          (z (+ x y)))
      (* z x)))
  35)

(test
  (let ((x 2)
        (y 3))
    (let* ((x 7)
           (z (+ x y)))
      (* z x)))
  70)

(test
  (letrec ((factorial
            (lambda (n)
              (if (zero? n) 1
                  (* n (factorial (- n 1)))))))
    (factorial 10))
   3628800)

(test
  (letrec ((even?
             (lambda (n)
               (if (zero? n) #true
                   (odd? (- n 1)))))
           (odd?
             (lambda (n)
               (if (zero? n) #false
                   (even? (- n 1))))))
    (even? 88))
   #true)


; ------------------------------------------------------------------------------
;   4.2.3 Sequencing
; ------------------------------------------------------------------------------

(define x 0)

(test
  (and (= x 0)
       (begin (set! x 5)
              (+ x 1)))
  6)

(begin (display "4 plus 1 equals ")
       (display (+ 4 1)))


; ------------------------------------------------------------------------------
;   4.2.4 Iteration
; ------------------------------------------------------------------------------

; (test
;   (do ((vec (make-vector 5))
;        (i 0 (+ i 1)))
;       ((= i 5) vec)
;     (vector-set! vec i i))
;   #(0 1 2 3 4))

; (test
;   (let ((x '(1 3 5 7 9)))
;     (do ((x x (cdr x))
;          (sum 0 (+ sum (car x))))
;         ((null? x) sum)))
;   25)

; ------------------------------------------------------------------------------
;   4.2.5 Delayed Evaluation
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
;   4.2.6 Dynamic Bindings
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
;   4.2.7 Exception Handling
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
;   4.2.8 Quasiquotation
; ------------------------------------------------------------------------------

(test
 `(list ,(+ 1 2) 4)
  (list 3 4))

(test
  (let ((name 'a))
   `(list ,name ',name))
  (list a (quote a)))

; (test
;  `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b) ; abs unimplemented
;   (a 3 4 5 6 b))

(test
 `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))
  ((foo 7) . cons))

; (test
;  `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)
;   #(10 5 2 4 3 8))

(test
  (let ((foo '(foo bar))
        (@baz 'baz))
   `(list ,@foo , @baz))
  (list foo bar baz))

; (test
;  `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
;   (a `(b ,(+ 1 2) ,(foo 4 d) e) f))

; (test
;   (let ((name1 'x)
;         (name2 'y))
;    `(a `(b ,,name1 ,',name2 d) e))
;   (a `(b ,x ,'y d) e))


; ------------------------------------------------------------------------------
;   4.2.9 Case-Lambda
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
;   4.3.1 Binding Constructs for Syntactic Keywords
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
;   4.3.2 Pattern Language
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
;   4.3.3 Signaling Errors in Macro Transformers
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
;   5.3.1 Top Level Definitions
; ------------------------------------------------------------------------------

(define add3
  (lambda (x) (+ x 3))); => add3
(add3 3); => 6

(define first car); => first
(first '(1 2)); => 1


; ------------------------------------------------------------------------------
;   5.3.2 Internal Definitions
; ------------------------------------------------------------------------------

(test
  (let ((x 5))
    (define foo
      (lambda (y)
        (bar x y)))
    (define bar
      (lambda (a b)
        (+ (* a b) a)))
    (foo (+ x 3)))
  45)

(test
  (let ((x 5))
    (letrec* ((foo
                (lambda (y)
                  (bar x y)))
              (bar
                (lambda (a b)
                  (+ (* a b) a))))
      (foo (+ x 3))))
  45)


; ------------------------------------------------------------------------------
;   6.1 Equivalence Predicates
; ------------------------------------------------------------------------------

(test
  (eqv? 'a
        'a)
  #true)

(test
  (eqv? 'a
        'b)
  #false)

(test
  (eqv? '()
        '())
  #true)

(test
  (eqv? 2
        2)
  #true)

; (test ; unimplemented
;   (eqv? 2 2.0)
;   #false)

(test
  (eqv? 100000000
        100000000)
  #true)

; (test ; unimplemented
;   (eqv? 0.0 +nan.0)
;   #false)

(test
  (eqv? (cons 1 2)
        (cons 1 2))
  #false)

(test
  (eqv? (lambda () 1)
        (lambda () 2))
  #false)

(test
  (let ((p (lambda (x) x)))
    (eqv? p p))
  #true)

(test
  (eqv? #false 'nil)
  #false)


; (eqv? "" ""); unspecified

; (eqv? '#() '#()); #unspecified

; (eqv? (lambda (x) x)
;       (lambda (x) x)); #unspecified

; (eqv? (lambda (x) x)
;       (lambda (x) y)); #unspecified

; (eqv? 1.0e0 1.0f0); #unspecified

; (eqv? +nan.0 +nan.0); #unspecified


(define generate-counter
  (lambda ()
    (let ((n 0))
      (lambda ()
        (set! n (+ n 1))
        n))))

(test
  (let ((g (generate-counter)))
    (eqv? g g))
  #true)

(test
  (eqv? (generate-counter)
        (generate-counter))
  #false)

(define generate-loser
  (lambda ()
    (let ((n 0))
      (lambda ()
        (set! n (+ n 1))
        27))))

(test
  (let ((g (generate-loser)))
    (eqv? g g))
  #true)

; (eqv? (generate-loser) (generate-loser)); #unspecified

; (letrec ((f (lambda () (if (eqv? f g) 'both 'f)))
;          (g (lambda () (if (eqv? f g) 'both 'g))))
;   (eqv? f g)); #unspecified

; (test; #unimplemented
;   (letrec ((f (lambda () (if (eqv? f g) ’f ’both)))
;            (g (lambda () (if (eqv? f g) ’g ’both))))
;     (eqv? f g))
;   #false)

; (eqv? '(a) '(a)); #unspecified
; (eqv? "a" "a"); #unspecified
; (eqv? '(b) (cdr '(a b))); #unspecified

(test
  (let ((x '(a)))
    (eqv? x x))
  #true)

(test
  (eq? 'a
       'a)
  #true)

; (eq? '(a) '(a)); #unspecified

(test
  (eq? (list 'a)
       (list 'a))
  #false)

; (eq? "a" "a"); #unspecified
; (eq? "" ""); #unspecified

(test
  (eq? '()
       '())
  #true)

; (eq? 2 2); #unspecified
; (eq? #\A #\A); #unspecified

(test
  (eq? car car)
  #true)

; (let ((n (+ 2 3)))
;   (eq? n n)); #unspecified

(test
  (let ((x '(a)))
    (eq? x x))
  #true)

; (test
;   (let ((x '#())) ; #unimplemented
;     (eq? x x))
;   #true)

(test
  (let ((p (lambda (x) x)))
    (eq? p p))
  #true)

(test
  (equal? 'a
          'a)
  #true)

(test
  (equal? '(a)
          '(a))
  #true)

(test
  (equal? '(a (b) c)
          '(a (b) c))
  #true)

; (test
;   (equal? "abc"
;           "abc") ; not fully supported yet
;   #true)

(test
  (equal? 2 2)
  #true)

; (equal? (make-vector 5 'a)
;         (make-vector 5 'a)); #true
;
; (equal? '#1=(a b . #1#)
;         '#2=(a b a b . #2#)); #true
;
; (equal? (lambda (x) x)
;         (lambda (y) y)); #unspecified


; ------------------------------------------------------------------------------
;   6.10 Control Features
; ------------------------------------------------------------------------------

; (test
;   (call-with-current-continuation
;     (lambda (exit)
;       (for-each (lambda (x)
;                   (if (negative? x)
;                       (exit x)))
;                '(54 0 37 -3 245 19))
;     #false))
;   -3)

(define list-length
  (lambda (object)
    (call-with-current-continuation
      (lambda (return)
        (begin (display "OK:1")
               (newline))
        (letrec ((r
                   (lambda (object)
                     (cond ((null? object) 0)
                           ((pair? object)
                            (+ (r (cdr object)) 1))
                           (else (return #false))))))
          (r object))))))

(test
  (list-length '(1 2 3 4))
  4)

(test
  (list-length '(a b . c))
  #false)


; ------------------------------------------------------------------------------
;   Miscellaneous
; ------------------------------------------------------------------------------

; (define x 42)
; x
; (set! x 100)
; x
;
; (define y 'hoge)
; y
; (set! y 100)
; y
;
; (define accumulator
;   (lambda (n)
;     (lambda ()
;       (set! n (+ n 1)))))
;
; (define acc (accumulator x))
; (acc)
; (acc)
; (acc)

(define A 1)
(define B 2)

(test
  (begin (swap! A B)
         (cons A B))
  (2 . 1))

(define x 42)

(test ; this test knows swap! uses 'x' as temporary variable.
  (begin (swap! A x)
         (cons A x))
  (42 . 2))

(begin (newline)
       (display "test ")
       (display test-passed)
       (display " expression passed (completed).")
       (newline)
       'completed)

