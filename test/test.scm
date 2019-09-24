(load "../test/expect.scm")

; ------------------------------------------------------------------------------
;   4.1.1 Variable References
; ------------------------------------------------------------------------------

(define x 28)

(expect 28 x)


; ------------------------------------------------------------------------------
;   4.1.2 Literal Expressions
; ------------------------------------------------------------------------------

(expect a
  (quote a))

; (expect
;   #(a b c)
;   (quote #(a b c))) ; unimplemented

(expect
  (+ 1 2)
  (quote (+ 1 2)))

(expect a 'a)

; (expect
;   #(a b c)
;  '#(a b c)) ; unimplemented

(expect () '())

(expect
  (+ 1 2)
 '(+ 1 2))

(expect
  (quote a)
 '(quote a))

(expect (quote a) ''a)

(expect 145932 '145932)
(expect 145932  145932)

; (expect "abc" '"abc")
; (expect "abc"  "abc") ; マクロ中で文字列の挙動がおかしいバグあり

; (expect # '#)
; (expect #  #) ; unimplemented

; (expect #(a 10) '#(a 10))
; (expect #(a 10)  #(a 10)) ; unimplemented

; (expect #u8(64 65) '#u8(64 65))
; (expect #u8(64 65)  #u8(64 65)) ; unimplemented

(expect #t '#t)
(expect #t  #t)


; ------------------------------------------------------------------------------
;   4.1.3 Procedure Calls
; ------------------------------------------------------------------------------

(expect 7 (+ 3 4))

(expect 12
  ((if #false + *) 3 4))


; ------------------------------------------------------------------------------
;   4.1.4 Procedures
; ------------------------------------------------------------------------------

; (lambda (x) (+ x x)) ; untestable output

(expect 8
  ((lambda (x) (+ x x)) 4))

(define reverse-subtract
  (lambda (x y)
    (- y x)))

(expect 3
  (reverse-subtract 7 10))

(define add4
  (let ((x 4))
    (lambda (y) (+ x y))))

(expect 10 (add4 6))

(expect (3 4 5 6)
  ((lambda x x) 3 4 5 6))

(expect (5 6)
  ((lambda (x y . z) z) 3 4 5 6))


; ------------------------------------------------------------------------------
;   4.1.5 Conditionals
; ------------------------------------------------------------------------------

(expect yes
  (if (> 3 2) 'yes 'no))

(expect no
  (if (> 2 3) 'yes 'no))

(expect 1
  (if (> 3 2)
      (- 3 2)
      (+ 3 2)))


; ------------------------------------------------------------------------------
;   4.1.6 Assignments
; ------------------------------------------------------------------------------

(define x 2)

(expect 3 (+ x 1))

(set! x 4) ; #unspecified

(expect 5
  (+ x 1))


; ------------------------------------------------------------------------------
;   4.2.1 Conditionals
; ------------------------------------------------------------------------------

(expect greater
  (cond ((> 3 2) 'greater)
        ((< 3 2) 'less)))

(expect equal
  (cond ((> 3 3) 'greater)
        ((< 3 3) 'less)
        (else 'equal)))

; (expect
;   (cond ((assv 'b '((a 1) (b 2))) => cadr)
;         (else #f))
;   2)

(expect composite
  (case (* 2 3)
    ((2 3 5 7) 'prime)
    ((1 4 6 8 9) 'composite)))

; (expect
;   (case (car '(c d))
;     ((a) 'a)
;     ((b) 'b))
;   undefined)

; (expect
;   (case (car '(c d))
;     ((a e i o u) 'vowel)
;     ((w y) 'semivowel)
;     (else => (lambda (x) x)))
;   c)

(expect #true
  (and (= 2 2)
       (> 2 1)))

(expect #false
  (and (= 2 2)
       (< 2 1)))

(expect (f g)
  (and 1 2 'c '(f g)))

(expect #true (and))

(expect #true
  (or (= 2 2)
      (> 2 1)))

(expect #true
  (or (= 2 2)
      (< 2 1)))

(expect #false
  (or #false #false #false))

; (expect
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

(expect 6
  (let ((x 2)
        (y 3))
    (* x y)))

(expect 35
  (let ((x 2)
        (y 3))
    (let ((x 7)
          (z (+ x y)))
      (* z x))))

(expect 70
  (let ((x 2)
        (y 3))
    (let* ((x 7)
           (z (+ x y)))
      (* z x))))

(expect 3628800
  (letrec ((factorial
            (lambda (n)
              (if (zero? n) 1
                  (* n (factorial (- n 1)))))))
    (factorial 10)))

(expect #true
  (letrec ((even?
             (lambda (n)
               (if (zero? n) #true
                   (odd? (- n 1)))))
           (odd?
             (lambda (n)
               (if (zero? n) #false
                   (even? (- n 1))))))
    (even? 88)))


; ------------------------------------------------------------------------------
;   4.2.3 Sequencing
; ------------------------------------------------------------------------------

(define x 0)

(expect 6
  (and (= x 0)
       (begin (set! x 5)
              (+ x 1))))

(begin (display "4 plus 1 equals ")
       (display (+ 4 1)))


; ------------------------------------------------------------------------------
;   4.2.4 Iteration
; ------------------------------------------------------------------------------

; (expect
;   (do ((vec (make-vector 5))
;        (i 0 (+ i 1)))
;       ((= i 5) vec)
;     (vector-set! vec i i))
;   #(0 1 2 3 4))

; (expect
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

(expect
  (list 3 4)
 `(list ,(+ 1 2) 4))

(expect
  (list a (quote a))
  (let ((name 'a))
   `(list ,name ',name)))

; (expect
;  `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b) ; abs unimplemented
;   (a 3 4 5 6 b))

(expect
  ((foo 7) . cons)
 `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))))

; (expect
;  `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)
;   #(10 5 2 4 3 8))

(expect
  (list foo bar baz)
  (let ((foo '(foo bar))
        (@baz 'baz))
   `(list ,@foo , @baz)))

; (expect
;  `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
;   (a `(b ,(+ 1 2) ,(foo 4 d) e) f))

; (expect
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
  (lambda (x)
    (+ x 3)))

(expect 6 (add3 3))

(define first car)

(expect 1 (first '(1 2)))


; ------------------------------------------------------------------------------
;   5.3.2 Internal Definitions
; ------------------------------------------------------------------------------

(expect 45
  (let ((x 5))
    (define foo
      (lambda (y)
        (bar x y)))
    (define bar
      (lambda (a b)
        (+ (* a b) a)))
    (foo (+ x 3))))

(expect 45
  (let ((x 5))
    (letrec* ((foo
                (lambda (y)
                  (bar x y)))
              (bar
                (lambda (a b)
                  (+ (* a b) a))))
      (foo (+ x 3)))))


; ------------------------------------------------------------------------------
;   6.1 Equivalence Predicates
; ------------------------------------------------------------------------------

(expect #true
  (eqv? 'a 'a))

(expect #false
  (eqv? 'a 'b))

(expect #true
  (eqv? '() '()))

(expect #true
  (eqv? 2 2))

; (expect ; unimplemented
;   (eqv? 2 2.0)
;   #false)

(expect #true
  (eqv? 100000000
        100000000))

; (expect ; unimplemented
;   (eqv? 0.0 +nan.0)
;   #false)

(expect #false
  (eqv? (cons 1 2)
        (cons 1 2)))

(expect #false
  (eqv? (lambda () 1)
        (lambda () 2)))

(expect #true
  (let ((p (lambda (x) x)))
    (eqv? p p)))

(expect #false
  (eqv? #false 'nil))


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

(expect #true
  (let ((g (generate-counter)))
    (eqv? g g)))

(expect #false
  (eqv? (generate-counter)
        (generate-counter)))

(define generate-loser
  (lambda ()
    (let ((n 0))
      (lambda ()
        (set! n (+ n 1))
        27))))

(expect #true
  (let ((g (generate-loser)))
    (eqv? g g)))

; (eqv? (generate-loser) (generate-loser)); #unspecified

; (letrec ((f (lambda () (if (eqv? f g) 'both 'f)))
;          (g (lambda () (if (eqv? f g) 'both 'g))))
;   (eqv? f g)); #unspecified

(expect #false
  (letrec ((f (lambda () (if (eqv? f g) ’f ’both)))
           (g (lambda () (if (eqv? f g) ’g ’both))))
    (eqv? f g)))

; (eqv? '(a) '(a)); #unspecified
; (eqv? "a" "a"); #unspecified
; (eqv? '(b) (cdr '(a b))); #unspecified

(expect #true
  (let ((x '(a)))
    (eqv? x x)))

(expect #true
  (eq? 'a 'a))

; (eq? '(a) '(a)); #unspecified

(expect #false
  (eq? (list 'a)
       (list 'a)))

; (eq? "a" "a"); #unspecified
; (eq? "" ""); #unspecified

(expect #true
  (eq? '()
       '()))

; (eq? 2 2); #unspecified
; (eq? #\A #\A); #unspecified

(expect #true
  (eq? car car))

; (let ((n (+ 2 3)))
;   (eq? n n)); #unspecified

(expect #true
  (let ((x '(a)))
    (eq? x x)))

; (expect
;   (let ((x '#())) ; #unimplemented
;     (eq? x x))
;   #true)

(expect #true
  (let ((p (lambda (x) x)))
    (eq? p p)))

(expect #true
  (equal? 'a 'a))

(expect #true
  (equal? '(a) '(a)))

(expect #true
  (equal? '(a (b) c)
          '(a (b) c)))

; (expect
;   (equal? "abc"
;           "abc") ; not fully supported yet
;   #true)

(expect #true
  (equal? 2 2))

; (equal? (make-vector 5 'a)
;         (make-vector 5 'a)); #true
;
; (equal? '#1=(a b . #1#)
;         '#2=(a b a b . #2#)); #true
;
; (equal? (lambda (x) x)
;         (lambda (y) y)); #unspecified


; ------------------------------------------------------------------------------
;   6.4 Pair and List
; ------------------------------------------------------------------------------

(expect
  (a b c d e)
  (a . (b . (c . (d . (e . ()))))))

(expect
  (a b c . d)
  (a . (b . (c . d))))


(expect (3 3)
  (make-list 2 3))


(expect (a 7 c)
  (list 'a (+ 3 4) 'c))

(expect ()
  (list))


(expect 3
  (length '(a b c)))

(expect 3
  (length '(a (b) (c d e))))

(expect 0
  (length '()))


(expect (x y)
  (append '(x) '(y)))

(expect (a b c d)
  (append '(a) '(b c d)))

(expect (a (b) (c))
  (append '(a (b)) '((c))))

(expect (a b c . d)
  (append '(a b) '(c . d)))

(expect a
  (append '() 'a))


(expect (c b a)
  (reverse '(a b c)))

(expect ((e (f)) d (b c) a)
  (reverse ’(a (b c) d (e (f)))))


(expect c
  (list-ref '(a b c d) 2))

; (expect c
;   (list-ref '(a b c d)
;              (exact (round 1.8))))


(expect (a b c)
  (memq 'a '(a b c)))

(expect (b c)
  (memq 'b '(a b c)))

(expect #false
  (memq 'a '(b c d)))

(expect #false
  (memq (list 'a) '(b (a) c)))

(expect ((a) c)
  (member (list 'a) '(b (a) c)))

; (expect ("B" "C")
;   (member "B" '("a" "b" "c") string-ci=?))

(expect #false ; #unspecified
  (memq 101 '(100 101 102)))

(expect (101 102)
  (memv 101 '(100 101 102)))


(define e '((a 1) (b 2) (c 3)))

(expect (a 1)
  (assq 'a e))

(expect (b 2)
  (assq 'b e))

(expect #false
  (assq 'd e))

(expect #false
  (assq (list 'a) '(((a)) ((b)) ((c)))))

(expect ((a))
  (assoc (list 'a) '(((a)) ((b)) ((c)))))

(expect (2 4)
  (assoc 2.0 '((1 1) (2 4) (3 9)) =))

(expect #false ; unspecified
  (assq 5 '((2 3) (5 7) (11 13))))

(expect (5 7)
  (assv 5 '((2 3) (5 7) (11 13))))


; ------------------------------------------------------------------------------
;   6.10 Control Features
; ------------------------------------------------------------------------------

(expect 7
  (apply + (list 3 4)))

(define compose
  (lambda (f g)
    (lambda args
      (f (apply g args)))))

; (expect 30
;   ((compose sqrt *) 12 75))

(expect (b e h)
  (map cadr '((a b) (d e) (g h))))

; (expect (1 4 27 256 3125)
;   (map (lambda (n)
;          (expt n n))
;       '(1 2 3 4 5)))

(expect (5 7 9)
  (map + '(1 2 3) '(4 5 6 7)))

(expect (1 2) ; or (2 1)
  (let ((count 0))
    (map (lambda (ignored)
           (set! count (+ count 1))
           count)
        '(a b))))

(expect -3
  (call-with-current-continuation
    (lambda (exit)
      (for-each (lambda (x)
                  (if (negative? x)
                      (exit x)))
               '(54 0 37 -3 245 19))
    #false)))

(define list-length
  (lambda (object)
    (call-with-current-continuation
      (lambda (return)
        (letrec ((r
                   (lambda (object)
                     (cond ((null? object) 0)
                           ((pair? object)
                            (+ (r (cdr object)) 1))
                           (else (return #false))))))
          (r object))))))

(expect 4
  (list-length '(1 2 3 4)))

(expect #false
  (list-length '(a b . c)))


(expect 5
  (call-with-values
    (lambda () (values 4 5))
    (lambda (a b) b)))

(expect -1
  (call-with-values * -))


; ------------------------------------------------------------------------------
;   Miscellaneous
; ------------------------------------------------------------------------------

(define x 42)
(expect 42 x)

(set! x 100)
(expect 100 x)

(define y 'hoge)
(expect hoge y)

(set! y 100)
(expect 100 y)


(define accumulator
  (lambda (n)
    (lambda ()
      (set! n (+ n 1)))))

(define acc (accumulator x))

(expect 101 (acc))
(expect 102 (acc))
(expect 103 (acc))

(define A 1)
(define B 2)

(expect (2 . 1)
  (begin (swap! A B)
         (cons A B)))

(define x 42)

(expect (42 . 2) ; this test knows swap! uses 'x' as temporary variable.
  (begin (swap! A x)
         (cons A x)))

; (define fib
;   (lambda (n)
;     (if (< n 2) n
;         (+ (fib (- n 1))
;            (fib (- n 2))))))
;
; (define fib-i
;   (lambda (n)
;     (if (< n 2)
;         (values 1 1)
;         (reverive (current previous)
;           (fib-i (- n 1))
;           (values (+ current previous) current)))))
;
; (fib 20)

(begin (newline)
       (display "test ")
       (display passed)
       (display " expression passed (completed).")
       (newline))

