(import (scheme base)
        (scheme char)
        (scheme eval)
        (scheme file)
        (scheme inexact)
        (scheme lazy)
        (scheme process-context)
        (scheme read)
        (scheme write)
        (only (scheme r5rs) null-environment)
        (srfi 78))

; ---- 1.3.4. ------------------------------------------------------------------

(check (* 5 8) => 40)

; ---- 2.1. --------------------------------------------------------------------

(check (symbol? '...) => #t)

(check (symbol? '+) => #t)

(check (symbol? '+soup+) => #t)

(check (symbol? '<=?) => #t)

(check (symbol? '->string) => #t)

(check (symbol? 'a34kTMNs) => #t)

(check (symbol? 'lambda) => #t)

(check (symbol? 'list->vector) => #t)

(check (symbol? 'q) => #t)

(check (symbol? 'V17a) => #t)

; (check (symbol? |two words|) => #t)

; (check (symbol? |two\x20;words|) => #t)

(check (symbol? 'the-word-recursion-has-many-meanings) => #t)

#!fold-case

#!no-fold-case

; ---- 2.2. --------------------------------------------------------------------

; #|
;   The FACT procedure computes the factorial
;   of a non-negative integer.
; |#
(define fact
  (lambda (n)
    (if (= n 0)
        #;(= n 1)
        1        ;Base case: return 1
        (* n (fact (- n 1))))))

; ---- 2.4. --------------------------------------------------------------------

; (check (let ((x (list 'a 'b 'c)))
;          (set-cdr! (cddr x) x)
;          x) => #0=(a b c . #0#)) ; TODO

; #1=(begin (display #\x) #1#) ; MUST BE ERROR

; ---- 4.1.1. ------------------------------------------------------------------

(define x 28)

(check x => 28)

; ---- 4.1.2. ------------------------------------------------------------------

(check (quote a) => a)

(check (quote #(a b c)) => #(a b c))

(check (quote (+ 1 2)) => (+ 1 2))

(check 'a => a)

(check '#(a b c) => #(a b c))

(check '() => ())

(check '(+ 1 2) => (+ 1 2))

(check '(quote a) => (quote a))

(check ''a  => (quote a))

(check '145932 => 145932)

(check 145932 => 145932)

(check '"abc" => "abc")

(check "abc" => "abc")

; (check '# => #)

; (check # => #)

(check '#(a 10) => #(a 10))

(check #(a 10) => #(a 10))

; (check '#u8(64 65) => #u8(64 65))

; (check #u8(64 65) => #u8(64 65))

(check '#t => #t)

(check #t => #t)

; ---- 4.1.3. ------------------------------------------------------------------

(check (+ 3 4) => 7)

(check ((if #f + *) 3 4) => 12)

; ---- 4.1.4. ------------------------------------------------------------------

(check (procedure? (lambda (x) (+ x x))) => #t)

(check ((lambda (x) (+ x x)) 4) => 8)

(define reverse-subtract
  (lambda (x y)
    (- y x)))

(check (reverse-subtract 7 10) => 3)

(define add4
  (let ((x 4))
    (lambda (y)
      (+ x y))))

(check (add4 6) => 10)

(check ((lambda x x) 3 4 5 6) => (3 4 5 6))

(check ((lambda (x y . z) z) 3 4 5 6) => (5 6))

; ---- 4.1.5. ------------------------------------------------------------------

(check (if (> 3 2) 'yes 'no) => yes)

(check (if (> 2 3) 'yes 'no) => no)

(check (if (> 3 2)
           (- 3 2)
           (+ 3 2)) => 1)

; ---- 4.1.6. ------------------------------------------------------------------

(define x 2)

(check (+ x 1) => 3)

(check (set! x 4) => 4)

(check (+ x 1) => 5)

; ---- 4.2.1. ------------------------------------------------------------------

(check (cond ((> 3 2) 'greater)
             ((< 3 2) 'less)) => greater)

(check (cond ((> 3 3) 'greater)
             ((< 3 3) 'less)
             (else 'equal)) => equal)

(check (cond ((assv 'b '((a 1) (b 2))) => cadr)
             (else #f)) => 2)

(check (case (* 2 3)
         ((2 3 5 7) 'prime)
         ((1 4 6 8 9) 'composite)) => composite)

(check (case (car '(c d))
         ((a) 'a)
         ((b) 'b)) => #,(if #f #f))

(check (case (car '(c d))
         ((a e i o u) 'vowel)
         ((w y) 'semivowel)
         (else => (lambda (x) x))) => c)

(check (and (= 2 2)
            (> 2 1)) => #t)

(check (and (= 2 2)
            (< 2 1)) => #f)

(check (and 1 2 'c '(f g)) => (f g))

(check (and) => #t)

(check (or (= 2 2)
           (> 2 1)) => #t)

(check (or (= 2 2)
           (< 2 1)) => #t)

(check (or #f #f #f) => #f)

(check (or (memq 'b '(a b c))
           (/ 3 0)) => (b c))

(check (when (= 1 1.0)
         (display "1")
         (display "2")) => #,(if #f #f))

(check (unless (= 1 1.0)
         (display "1")
         (display "2")) => #,(if #f #f))

; ---- 4.2.2. ------------------------------------------------------------------

(check (let ((x 2)
             (y 3))
         (* x y)) => 6)

(check (let ((x 2)
             (y 3))
         (let ((x 7)
               (z (+ x y)))
           (* z x))) => 35)

(check (let ((x 2)
             (y 3))
         (let* ((x 7)
                (z (+ x y)))
           (* z x))) => 70)

(check (letrec ((even?
                  (lambda (n)
                    (if (zero? n)
                        #t
                        (odd? (- n 1)))))
                (odd?
                  (lambda (n)
                    (if (zero? n)
                        #f
                        (even? (- n 1))))))
         (even? 88)) => #t)

(check (letrec* ((p (lambda (x)
                      (+ 1 (q (- x 1)))))
                 (q (lambda (y)
                      (if (zero? y)
                          0
                          (+ 1 (p (- y 1))))))
                 (x (p 5))
                 (y x))
         y) => 5)

; (check (let-values (((root rem)
;                      (exact-integer-sqrt 32)))
;          (* root rem)) => 35)

(check (let ((a 'a)
             (b 'b)
             (x 'x)
             (y 'y))
         (let*-values (((a b) (values x y))
                       ((x y) (values a b)))
           (list a b x y))) => (x y x y))

; ---- 4.2.3. ------------------------------------------------------------------

(define x 0)

(check (and (= x 0)
            (begin (set! x 5)
                   (+ x 1))) => 6)

(check (begin (display "4 plus 1 equals ")
              (display (+ 4 1))) => #,(if #f #f))

; ---- 4.2.4. ------------------------------------------------------------------

(check (do ((vec (make-vector 5))
            (i 0 (+ i 1)))
           ((= i 5) vec)
         (vector-set! vec i i)) => #(0 1 2 3 4))

(check (let ((x '(1 3 5 7 9)))
         (do ((x x (cdr x))
              (sum 0 (+ sum (car x))))
           ((null? x) sum))) => 25)

(check (let loop ((numbers '(3 -2 1 6 -5))
                  (nonneg '())
                  (neg '()))
         (cond ((null? numbers) (list nonneg neg))
               ((<= 0 (car numbers))
                (loop (cdr numbers)
                      (cons (car numbers) nonneg)
                      neg))
               ((< (car numbers) 0)
                (loop (cdr numbers)
                      nonneg
                      (cons (car numbers) neg))))) => ((6 1 3) (-5 -2)))

; ---- 4.2.5. ------------------------------------------------------------------

(check (force (delay (+ 1 2))) => 3)

(check (let ((p (delay (+ 1 2))))
         (list (force p)
               (force p))) => (3 3))

(define integers
  (letrec ((next (lambda (n)
                   (delay (cons n (next (+ n 1)))))))
    (next 0)))

(define head
  (lambda (stream)
    (car (force stream))))

(define tail
  (lambda (stream)
    (cdr (force stream))))

(check (head (tail (tail integers))) => 2)

(define (stream-filter p? s)
  (delay-force
    (if (null? (force s))
        (delay '())
        (let ((h (car (force s)))
              (t (cdr (force s))))
          (if (p? h)
              (delay (cons h (stream-filter p? t)))
              (stream-filter p? t))))))

(check (head (tail (tail (stream-filter odd? integers)))) => 5)

(define count 0)

(define p
  (delay (begin (set! count (+ count 1))
                (if (> count x)
                    count
                    (force p)))))

(define x 5)

(check (promise? p) => #t)

(check (force p) => 6)

(check (promise? p) => #t)

(check (begin (set! x 10)
              (force p)) => 6)

(check (eqv? (delay 1) 1) => #f) ; unspecified

(check (pair? (delay (cons 1 2))) => #t) ; unspecified

; (check (+ (delay (* 3 7)) 13) => ???)

; (check (car (list (delay (* 3 7)) 13)) => ???)

; ---- 4.2.6. ------------------------------------------------------------------

(define radix
  (make-parameter 10
    (lambda (x)
      (if (and (exact-integer? x)
               (<= 2 x 16))
          x
          (error "invalid radix")))))

(define (f n)
  (number->string n (radix)))

; (check (f 12) => "12")

; (parameterize ((radix 2))
;   (check (f 12) => "1100")

; (check (f 12) => "12")

; (check (radix 16) => ???)

; (parameterize ((radix 0))
;   (f 12)) ; => error

; ---- 4.2.7. ------------------------------------------------------------------

; (check (guard (condition
;                 ((assq 'a condition) => cdr)
;                 ((assq 'b condition)))
;          (raise (list (cons 'a 42)))) => 42)

; (check (guard (condition
;                 ((assq 'a condition) => cdr)
;                 ((assq 'b condition)))
;          (raise (list (cons 'b 23)))) => (b . 23))

; ---- 4.2.8. ------------------------------------------------------------------

(check `(list ,(+ 1 2) 4) => (list 3 4))

(check (let ((name 'a))
         `(list ,name ',name)) => (list a (quote a)))

(check `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b) => (a 3 4 5 6 b))

(check `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))) => ((foo 7) . cons))

(check `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8) => #(10 5 2 4 3 8))

(check `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f) => (a `(b ,(+ 1 2) ,(foo 4 d) e) f))

(check (let ((name1 'x)
             (name2 'y))
         `(a `(b ,,name1 ,',name2 d) e)) => (a `(b ,x ,'y d) e))

(check (let ((a 3))
         `((1 2) ,a ,4 ,'five 6)) => ((1 2) 3 4 five 6))

(check (quasiquote (list (unquote (+ 1 2)) 4)) => (list 3 4))

(check '(quasiquote (list (unquote (+ 1 2)) 4)) => `(list ,(+ 1 2) 4))

; ---- 4.2.9. ------------------------------------------------------------------

; (define range
;   (case-lambda
;     ((e) (range 0 e))
;     ((b e) (do ((r ’() (cons e r))
;                 (e (- e 1) (- e 1)))
;                ((< e b) r)))))

; (check (range 3) => (0 1 2))

; (check (range 3 5) => (3 4))

; ---- 4.3.1. ------------------------------------------------------------------

(check (let-syntax ((given-that (syntax-rules ()
                                  ((given-that test stmt1 stmt2 ...)
                                   (if test
                                       (begin stmt1
                                              stmt2 ...))))))
         (let ((if #t))
           (given-that if (set! if 'now))
           if)) => now)

(check (let ((x 'outer))
         (let-syntax ((m (syntax-rules ()
                           ((m) x))))
           (let ((x 'inner))
             (m)))) => outer)

(check (letrec-syntax ((my-or (syntax-rules ()
                                ((my-or) #f)
                                ((my-or e) e)
                                ((my-or e1 e2 ...)
                                 (let ((temp e1))
                                   (if temp
                                       temp
                                       (my-or e2 ...)))))))
         (let ((x #f)
               (y 7)
               (temp 8)
               (let odd?)
               (if even?))
           (my-or x
                  (let temp)
                  (if y)
                  y))) => 7)

; ---- 4.3.2. ------------------------------------------------------------------

(define-syntax be-like-begin
  (syntax-rules ()
    ((be-like-begin name)
     (define-syntax name
       (syntax-rules ()
         ((name expr (... ...))
          (begin expr (... ...))))))))

(be-like-begin sequence)

(check (sequence 1 2 3 4) => 4)

(check (let ((=> #f))
         (cond (#t => 'ok))) => ok)

(define-syntax simple-let
  (syntax-rules ()
    ((_ (head ... ((x . y) val) . tail) body1 body2 ...)
     (syntax-error "expected an identifier but got" (x . y)))
    ((_ ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...) val ...))))

; ---- 5.3.1. ------------------------------------------------------------------

(define add3
  (lambda (x)
    (+ x 3)))

(check (add3 3) => 6)

(define first car)

(check (first '(1 2)) => 1)

; ---- 5.3.2. ------------------------------------------------------------------

(check (let ((x 5))
         (define foo
           (lambda (y)
             (bar x y)))
         (define bar
           (lambda (a b)
             (+ (* a b) a)))
         (foo (+ x 3))) => 45)

(check (let ((x 5))
         (letrec* ((foo (lambda (y)
                          (bar x y)))
                   (bar (lambda (a b)
                          (+ (* a b) a))))
           (foo (+ x 3)))) => 45)

; ---- 5.3.3. ------------------------------------------------------------------

; (define-values (x y) (integer-sqrt 17))

; (check (list x y) => (4 1))

; (check (let ()
;          (define-values (x y)
;            (values 1 2))
;          (+ x y)) => 3)

; ---- 5.4. --------------------------------------------------------------------

; (check (let ((x 1)
;              (y 2))
;          (define-syntax swap!
;            (syntax-rules ()
;              ((swap! a b)
;               (let ((tmp a))
;                 (set! a b)
;                 (set! b tmp)))))
;          (swap! x y)
;          (list x y)) => (2 1))

; (define define 3) ; error

; (begin (define begin list)) ; error

; (check (let-syntax ((foo (syntax-rules ()
;                            ((foo (proc args ...) body ...)
;                             (define proc
;                               (lambda (args ...) body ...))))))
;          (let ((x 3))
;            (foo (plus x y)
;                 (+ x y))
;            (define foo x)
;            (plus foo x))) ; error

; ---- 5.5. --------------------------------------------------------------------

; (define-record-type <pare>
;   (kons x y)
;   pare?
;   (x kar set-kar!)
;   (y kdr))

; (check (pare? (kons 1 2)) => #t)

; (check (pare? (cons 1 2)) => #f)

; (check (kar (kons 1 2)) => 1)

; (check (kdr (kons 1 2)) => 2)

; (check (let ((k (kons 1 2)))
;          (set-kar! k 3)
;          (kar k)) => 3)

; ---- 5.6.2. ------------------------------------------------------------------

(define-library (example grid)
  (export make rows cols ref each (rename put! set!))
  (import (scheme base))
  (begin (define (make n m) ; Create an NxM grid.
           (let ((grid (make-vector n)))
             (do ((i 0 (+ i 1)))
                 ((= i n) grid)
               (let ((v (make-vector m #false)))
                 (vector-set! grid i v)))))
         (define (rows grid)
           (vector-length grid))
         (define (cols grid)
           (vector-length (vector-ref grid 0)))
         (define (ref grid n m) ; Return #false if out of range.
           (and (< -1 n (rows grid))
                (< -1 m (cols grid))
                (vector-ref (vector-ref grid n) m)))
         (define (put! grid n m v)
           (vector-set! (vector-ref grid n) m v))
         (define (each grid proc)
           (do ((j 0 (+ j 1)))
               ((= j (rows grid)))
             (do ((k 0 (+ k 1)))
                 ((= k (cols grid)))
               (proc j k (ref grid j k)))))))

(define-library (example life)
  (export life)
  (import (except (scheme base) set!)
          (scheme write)
          (example grid))
  (begin (define (life-count grid i j)
           (define (count i j)
             (if (ref grid i j) 1 0))
           (+ (count (- i 1) (- j 1))
              (count (- i 1)    j   )
              (count (- i 1) (+ j 1))
              (count    i    (- j 1))
              (count    i    (+ j 1))
              (count (+ i 1) (- j 1))
              (count (+ i 1)    j   )
              (count (+ i 1) (+ j 1))))
         (define (life-alive? grid i j)
           (case (life-count grid i j)
             ((3) #true)
             ((2) (ref grid i j))
             (else #false)))
         (define (life-print grid)
           (display "\x1B;[1H\x1B;[J") ; clear vt100
           (each grid
                 (lambda (i j v)
                   (display (if v "*" " "))
                   (when (= j (- (cols grid) 1))
                     (newline)))))
         (define (life grid iterations)
           (do ((i 0 (+ i 1))
                (grid0 grid grid1)
                (grid1 (make (rows grid) (cols grid)) grid0))
             ((= i iterations))
             (each grid0
                   (lambda (j k v)
                     (let ((a (life-alive? grid0 j k)))
                       (set! grid1 j k a))))
             (life-print grid1)))))

(import ; (scheme base)
        (only (example life) life)
        (rename (prefix (example grid) grid-)
                (grid-make make-grid)))

; (define grid (make-grid 24 24))

; (grid-set! grid 1 1 #true)

; (grid-set! grid 2 2 #true)

; (grid-set! grid 3 0 #true)

; (grid-set! grid 3 1 #true)

; (grid-set! grid 3 2 #true)

; (life grid 80)

; ---- 6.1. --------------------------------------------------------------------

(check (eqv? 'a 'a) => #t)

(check (eqv? 'a 'b) => #f)

(check (eqv? 2 2) => #t)

(check (eqv? 2 2.0) => #f)

(check (eqv? '() '()) => #t)

(check (eqv? 100000000 100000000) => #t)

(check (eqv? 0.0 +nan.0) => #f)

(check (eqv? (cons 1 2)
             (cons 1 2)) => #f)

(check (eqv? (lambda () 1)
             (lambda () 2)) => #f)

(check (let ((p (lambda (x) x)))
         (eqv? p p)) => #t)

(check (eqv? #f 'nil) => #f)

(check (eqv? "" "") => #t) ; unspecified

(check (eqv? '#() '#()) => #t) ; unspecified

(check (eqv? (lambda (x) x)
             (lambda (x) x)) => #f) ; unspecified

(check (eqv? (lambda (x) x)
             (lambda (y) y)) => #f) ; unspecified

(check (eqv? 1.0e0 1.0f0) => #t) ; unspecified

(check (eqv? +nan.0 +nan.0) => #t) ; unspecified

(define generate-counter
  (lambda ()
    (let ((n 0))
      (lambda ()
        (set! n (+ n 1)) n))))

(check (let ((g (generate-counter)))
         (eqv? g g)) => #t)

(check (eqv? (generate-counter)
             (generate-counter)) => #f)

(define generate-loser
  (lambda ()
    (let ((n 0))
      (lambda ()
        (set! n (+ n 1)) 27))))

(check (let ((g (generate-loser)))
         (eqv? g g)) => #t)

(check (eqv? (generate-loser)
             (generate-loser)) => #f) ; unspecified

(check (letrec ((f (lambda ()
                     (if (eqv? f g)
                         'both
                         'f)))
                (g (lambda ()
                     (if (eqv? f g)
                         'both
                         'g))))
         (eqv? f g)) => #f) ; unspecified

(check (letrec ((f (lambda ()
                     (if (eqv? f g)
                         'f
                         'both)))
                (g (lambda ()
                     (if (eqv? f g)
                         'g
                         'both))))
         (eqv? f g)) => #f)

(check (eqv? '(a) '(a)) => #t) ; unspecified

(check (eqv? "a" "a") => #t) ; unspecified

(check (eqv? '(b) (cdr '(a b))) => #t) ; unspecified

(check (let ((x '(a)))
         (eqv? x x)) => #t)

(check (eq? 'a 'a) => #t)

(check (eq? '(a) '(a)) => #f) ; unspecified

(check (eq? (list 'a)
            (list 'a)) => #f)

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

; (check (equal? '#1=(a b . #1#)
;                '#2=(a b a b . #2#)) => #t)

(check (equal? (lambda (x) x)
               (lambda (y) y)) => #f) ; unspecified

; ---- 6.2.6. ------------------------------------------------------------------

; (check (complex? 3+4i) => #t)

(check (complex? 3) => #t)

(check (real? 3) => #t)

; (check (real? -2.5+0i) => #t)

; (check (real? -2.5+0.0i) => #t)

(check (real? #e1e10) => #t)

(check (real? +inf.0) => #t)

(check (real? +nan.0) => #t)

(check (rational? -inf.0) => #f)

(check (rational? 3.5) => #t)

(check (rational? 6/10) => #t)

(check (rational? 6/3) => #t)

; (check (integer? 3+0i) => #t)

(check (integer? 3.0) => #t)

(check (integer? 8/4) => #t)

(check (exact? 3.0) => #f)

(check (exact? #e3.0) => #t)

(check (inexact? 3.) => #t)

(check (exact-integer? 32) => #t)

(check (exact-integer? 32.0) => #f)

(check (exact-integer? 32/5) => #f)

(check (finite? 3) => #t)

(check (finite? +inf.0) => #f)

; (check (finite? 3.0+inf.0i) => #f)

(check (infinite? 3) => #f)

(check (infinite? +inf.0) => #t)

(check (infinite? +nan.0) => #f)

; (check (infinite? 3.0+inf.0i) => #t)

(check (nan? +nan.0) => #t)

(check (nan? 32) => #f)

; (check (nan? +nan.0+5.0i) => #t)

; (check (nan? 1+2i) => #f)

(check (zero? 0/1) => #t)

(check (max 3 4) => 4) ; exact

(check (max 3.9 4) => 4.0) ; inexact

(check (+ 3 4) => 7)

(check (+ 3) => 3)

(check (+) => 0)

(check (* 4) => 4)

(check (*) => 1)

(check (- 3 4) => -1)

(check (- 3 4 5) => -6)

(check (- 3) => -3)

(check (/ 3 4 5) => 3/20)

(check (/ 3) => 1/3)

(check (abs -7) => 7)

(check (floor/ 5 2) => #,(values 2 1))

(check (floor/ -5 2) => #,(values -3 1))

(check (floor/ 5 -2) => #,(values -3 -1))

(check (floor/ -5 -2) => #,(values 2 -1))

(check (truncate/ 5 2) => #,(values 2 1))

(check (truncate/ -5 2) => #,(values -2 -1))

(check (truncate/ 5 -2) => #,(values -2 1))

(check (truncate/ -5 -2) => #,(values 2 -1))

(check (truncate/ -5.0 -2) => #,(values 2.0 -1.0))

(check (gcd 32 -36) => 4)

(check (gcd) => 0)

(check (lcm 32 -36) => 288)

(check (lcm 32.0 -36) => 288.0) ; inexact

(check (lcm) => 1)

(check (numerator (/ 6 4)) => 3)

(check (denominator (/ 6 4)) => 2)

(check (denominator (inexact (/ 6 4))) => 2.0)

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

(check (rationalize (exact .3) 1/10) => 1/3) ; exact

(check (rationalize .3  1/10) => #i1/3) ; inexact

(check (square 42) => 1764)

(check (square 2.0) => 4.0)

(check (sqrt 9) => 3)

; (check (sqrt -1) => +i)

; (check (exact-integer-sqrt 4) => #,(values 2 0))

; (check (exact-integer-sqrt 5) => #,(values 2 1))

(check (string->number "100") => 100)

(check (string->number "100" 16) => 256)

(check (string->number "1e2") => 100.0)

; ---- 6.3. --------------------------------------------------------------------

(check #t => #t)

(check #f => #f)

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

; ---- 6.4. --------------------------------------------------------------------

(check (equal? '(a b c d e)
               '(a . (b . (c . (d . (e . ())))))) => #t)

(check (equal? '(a b c . d)
               '(a . (b . (c . d)))) => #t)

(define x (list 'a 'b 'c))

(define y x)

(check y => (a b c))

(check (list? y) => #t)

(check (set-cdr! x 4) => 4)

(check x => (a . 4))

(check (eqv? x y) => #t)

(check y => (a . 4))

(check (list? y) => #f)

(set-cdr! x x)

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

(check (car '()) => ())

(check (cdr '((a) b c d)) => (b c d))

(check (cdr '(1 . 2)) => 2)

(check (cdr '()) => ())

(define (f)
  (list 'not-a-constant-list))

(define (g) '(constant-list))

(check (set-car! (f) 3) => 3)

(check (set-car! (g) 3) => 3)

(check (list? '(a b c)) => #t)

(check (list? '()) => #t)

(check (list? '(a . b)) => #f)

(check (let ((x (list 'a)))
         (set-cdr! x x)
         (list? x)) => #f)

(check (make-list 2 3) => (3 3))

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

(check (list-ref '(a b c d) (exact (round 1.8))) => c)

(check (let ((ls (list 'one 'two 'five!)))
         (list-set! ls 2 'three)
         ls) => (one two three))

(check (list-set! '(0 1 2) 1 "oops") => "oops")

(check (memq 'a '(a b c)) => (a b c))

(check (memq 'b '(a b c)) => (b c))

(check (memq 'a '(b c d)) => #f)

(check (memq (list 'a) '(b (a) c)) => #f)

(check (member (list 'a) '(b (a) c)) => ((a) c))

(check (member "B" '("a" "b" "c") string-ci=?) => ("b" "c"))

(check (memq 101 '(100 101 102)) => #f) ; unspecified

(check (memv 101 '(100 101 102)) => (101 102))

(define e '((a 1) (b 2) (c 3)))

(check (assq 'a e) => (a 1))

(check (assq 'b e) => (b 2))

(check (assq 'd e) => #f)

(check (assq (list 'a) '(((a)) ((b)) ((c)))) => #f)

(check (assoc (list 'a) '(((a)) ((b)) ((c)))) => ((a)))

(check (assoc 2.0 '((1 1) (2 4) (3 9)) =) => (2 4))

(check (assq 5 '((2 3) (5 7) (11 13))) => #f) ; unspecified

(check (assv 5 '((2 3) (5 7) (11 13))) => (5 7))

(define a '(1 8 2 8)) ; a may be immutable

(define b (list-copy a))

(set-car! b 3) ; b is mutable

(check b => (3 8 2 8))

(check a => (1 8 2 8))

; ---- 6.5. --------------------------------------------------------------------

(check (symbol? 'foo) => #t)

(check (symbol? (car '(a b))) => #t)

(check (symbol? "bar") => #f)

(check (symbol? 'nil) => #t)

(check (symbol? '()) => #f)

(check (symbol? #f) => #f)

(check (symbol->string 'flying-fish) => "flying-fish")

(check (symbol->string 'Martin) => "Martin")

(check (symbol->string (string->symbol "Malvina")) => "Malvina")

(check (string->symbol "mISSISSIppi") => mISSISSIppi)

(check (eqv? 'bitBlt (string->symbol "bitBlt")) => #t)

(check (eqv? 'LollyPop
             (string->symbol
               (symbol->string 'LollyPop))) => #t)

(check (string=? "K. Harper, M.D."
                 (symbol->string
                   (string->symbol "K. Harper, M.D."))) => #t)

; ---- 6.6. --------------------------------------------------------------------

(check (char? #\alarm) => #t) ; U+0007

(check (char? #\backspace) => #t) ; U+0008

(check (char? #\delete) => #t) ; U+007F

(check (char? #\escape) => #t) ; U+001B

(check (char? #\newline) => #t) ; U+000A

(check (char? #\null) => #t) ; U+0000

(check (char? #\return) => #t) ; U+000D

(check (char? #\space) => #t) ; U+0020

(check (char? #\tab) => #t) ; U+0009

(check (char? #\a) => #t)

(check (char? #\A) => #t)

(check (char? #\() => #t)

(check (char? #\ ) => #t)

(check (char? #\x03BB) => #t)

; (check (char? #\iota) => #t)

(check (char-ci=? #\A #\a) => #t)

(check (digit-value #\3) => 3)

; (check (digit-value #\x0664) => 4)

; (check (digit-value #\x0AE6) => 0)

; (check (digit-value #\x0EA6) => #f) ; BUG: MEMORY-LEAK

; ---- 6.7. --------------------------------------------------------------------

(check (string? "The word \"recursion\" has many meanings.") => #t)

(check (string? "Another example:\ntwo lines of test") => #t)

(check (string? "Here's test \
                   containing just one line") => #t)

(check (string? "\x03B1; is named GREEK SMALL LETTER ALPHA.") => #t)

(define (f) (make-string 3 #\*))

(define (g) "***")

(check (string-set! (f) 0 #\?) => "?**")

(check (string-set! (g) 0 #\?) => "?**")

(check (string-set! (symbol->string 'immutable) 0 #\?) => "?mmutable")

(define a "12345")

(define b (string-copy "abcde"))

; (string-copy! b 1 a 0 2)

; (check b => "a12de")

; ---- 6.8. --------------------------------------------------------------------

(check (vector? #(0 (2 2 2 2) "Anna")) => #t)

(check (vector 'a 'b 'c) => #(a b c))

(check (vector-ref '#(1 1 2 3 5 8 13 21) 5) => 8)

(check (vector-ref '#(1 1 2 3 5 8 13 21)
                   (exact (round (* 2 (acos -1))))) => 13)

(check (let ((vec (vector 0 '(2 2 2 2) "Anna")))
         (vector-set! vec 1 '("Sue" "Sue")) vec) => #(0 ("Sue" "Sue") "Anna"))

(check (vector-set! '#(0 1 2) 1 "doe") => "doe")

(check (vector->list '#(dah dah didah)) => (dah dah didah))

(check (vector->list '#(dah dah didah) 1 2) => (dah))

(check (list->vector '(dididit dah)) => #(dididit dah))

; (check (string->vector "ABC") => #(#\A #\B #\C))

(check (vector->string #(#\1 #\2 #\3)) => "123")

(define a #(1 8 2 8)) ; a may be immutable

(define b (vector-copy a))

(vector-set! b 0 3) ; b is mutable

(check b => #(3 8 2 8))

(define c (vector-copy b 1 3))

(check c => #(8 2))

(define a (vector 1 2 3 4 5))

(define b (vector 10 20 30 40 50))

(vector-copy! b 1 a 0 2)

(check b => #(10 1 2 40 50))

; (check (vector-append #(a b c) #(d e f)) => #(a b c d e f))

(define a (vector 1 2 3 4 5))

(vector-fill! a 'smash 2 4)

(check a => #(1 2 smash smash 5))

; ---- 6.9. --------------------------------------------------------------------

; (check (bytevector? #u8(0 10 5)) => #t)

; (check (make-bytevector 2 12) => #u8(12 12))

; (check (bytevector 1 3 5 1 3 5) => #u8(1 3 5 1 3 5))

; (check (bytevector) => #u8())

; (check (bytevector-u8-ref '#u8(1 1 2 3 5 8 13 21) 5) => 8)

; (check (let ((bv (bytevector 1 2 3 4)))
;          (bytevector-u8-set! bv 1 3)
;          bv) => #u8(1 3 3 4))

; (define a #u8(1 2 3 4 5))

; (check (bytevector-copy a 2 4) => #u8(3 4))

; (define a (bytevector 1 2 3 4 5))

; (define b (bytevector 10 20 30 40 50))

; (bytevector-copy! b 1 a 0 2)

; (check b => #u8(10 1 2 40 50))

; (check (bytevector-append #u8(0 1 2) #u8(3 4 5)) => #u8(0 1 2 3 4 5))

; (check (utf8->string #u8(#x41)) => "A")

; (check (string->utf8 "λ") => #u8(#xCE #xBB))

; ---- 6.10. -------------------------------------------------------------------

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

(check (map (lambda (n) (expt n n)) '(1 2 3 4 5)) => (1 4 27 256 3125))

(check (map + '(1 2 3) '(4 5 6 7)) => (5 7 9))

(check (let ((count 0))
         (map (lambda (ignored)
                (set! count (+ count 1))
                count)
              '(a b c))) => (1 2 3))

(check (string-map char-foldcase "AbdEgH") => "abdegh")

(check (string-map (lambda (c)
                     (integer->char (+ 1 (char->integer c))))
                   "HAL") => "IBM")

; (check (string-map (lambda (c k)
;                      ((if (eqv? k #\u) char-upcase char-downcase)
;                       c))
;                    "studlycaps xxx"
;                    "ululululul") => "StUdLyCaPs")

; (check (vector-map cadr '#((a b) (d e) (g h))) => #(b e h))

; (check (vector-map (lambda (n)
;                      (expt n n))
;                    '#(1 2 3 4 5)) => (1 4 27 256 3125))

; (check (vector-map + '#(1 2 3) '#(4 5 6 7)) => #(5 7 9))

; (check (let ((count 0))
;          (vector-map
;            (lambda (ignored)
;              (set! count (+ count 1))
;              count)
;            '#(a b))) => #(1 2)) ; or #(2 1)

(check (let ((v (make-vector 5)))
         (for-each (lambda (i)
                     (vector-set! v i (* i i)))
                   '(0 1 2 3 4))
         v) => #(0 1 4 9 16))

; (check (let ((v '()))
;          (string-for-each
;            (lambda (c)
;              (set! v (cons (char->integer c) v)))
;            "abcde")
;          v) => #(101 100 99 98 97))

; (check (let ((v (make-list 5)))
;          (vector-for-each
;            (lambda (i) (list-set! v i (* i i)))
;            '#(0 1 2 3 4))
;          v) => (0 1 4 9 16))

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
        (letrec ((r (lambda (object)
                      (cond ((null? object) 0)
                            ((pair? object)
                             (+ (r (cdr object)) 1))
                            (else (return #f))))))
          (r object))))))

(check (list-length '(1 2 3 4)) => 4)

(check (list-length '(a b . c)) => #f)

(check (call-with-values (lambda () (values 4 5))
                         (lambda (a b) b)) => 5)

(check (call-with-values * -) => -1)

(check (let ((path '())
             (c #f))
         (let ((add (lambda (s)
                      (set! path (cons s path)))))
           (dynamic-wind
             (lambda () (add 'connect))
             (lambda ()
               (add (call-with-current-continuation
                      (lambda (c0)
                        (set! c c0)
                        'talk1))))
             (lambda () (add 'disconnect)))
           (if (< (length path) 4)
               (c 'talk2)
               (reverse path)))) => (connect talk1 disconnect
                                     connect talk2 disconnect))

; ---- 6.11. -------------------------------------------------------------------

(check (call-with-current-continuation
         (lambda (k)
           (with-exception-handler
             (lambda (x)
               (display "condition: ")
               (write x)
               (newline)
               (k 'exception))
             (lambda ()
               (+ 1 (raise 'an-error)))))) => exception)

; (with-exception-handler
;   (lambda (x)
;     (display "something went wrong\n"))
;   (lambda ()
;     (+ 1 (raise 'an-error))))

(check (with-exception-handler
         (lambda (con)
           (cond ((string? con) (display con))
                 (else (display "a warning has been issued")))
           42)
         (lambda ()
           (+ (raise-continuable "should be a number")
              23)))
       => 65)

(define (null-list? l)
  (cond ((pair? l) #f)
        ((null? l) #t)
        (else (error "null-list?: argument out of domain" l))))

; ---- 6.12. -------------------------------------------------------------------

(check (eval '(* 7 3) (environment '(scheme base))) => 21)

(check (let ((f (eval '(lambda (f x) (f x x))
                      (null-environment 5))))
         (f + 10)) => 20)

(check (eval '(define foo 32) (environment '(scheme base))) => 32)

; ---- 6.13.1. -----------------------------------------------------------------

(check (parameterize ((current-output-port (open-output-string)))
         (display "piece")
         (display " by piece ")
         (display "by piece.")
         (newline)
         (get-output-string (current-output-port))) => "piece by piece by piece.\n")

; ------------------------------------------------------------------------------

(check-report)

(exit (check-passed? 376))
