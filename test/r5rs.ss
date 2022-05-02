; ---- 1.3.4 -------------------------------------------------------------------

(check (* 5 8) => 40)

; ---- 4.1.1 -------------------------------------------------------------------

(define x 28)

(check x => 28)

; ---- 4.1.2 -------------------------------------------------------------------

(check (quote a) => a)

(check (quote #(a b c)) => #(a b c))

(check (quote (+ 1 2)) => (+ 1 2))

(check 'a => a)

(check '#(a b c) => #(a b c))

(check '() => ())

(check '(+ 1 2) => (+ 1 2))

(check '(quote a) => (quote a))

(check ''a  => (quote a))

(check '"abc" => "abc")

(check "abc" => "abc")

(check '145932 => 145932)

(check 145932 => 145932)

(check '#t => #t)

(check #t => #t)

; ---- 4.1.3 -------------------------------------------------------------------

(check (+ 3 4) => 7)

(check ((if #f + *) 3 4) => 12)

; ---- 4.1.4 -------------------------------------------------------------------

(check (procedure? (lambda (x) (+ x x))) => #t)

(check ((lambda (x) (+ x x)) 4) => 8)

(define reverse-subtract
  (lambda (x y)
    (- y x)))

(check (reverse-subtract 7 10) => 3)

(define add4
  (let ((x 4))
    (lambda (y) (+ x y))))

(check (add4 6) => 10)

(check ((lambda x x) 3 4 5 6) => (3 4 5 6))

(check ((lambda (x y . z) z) 3 4 5 6) => (5 6))

; ---- 4.1.5 -------------------------------------------------------------------

(check (if (> 3 2) 'yes 'no) => yes)

(check (if (> 2 3) 'yes 'no) => no)

(check (if (> 3 2)
           (- 3 2)
           (+ 3 2)) => 1)

; ---- 4.1.6 -------------------------------------------------------------------

(define x 2)

(check (+ x 1) => 3)

(check (set! x 4) => 4)

(check (+ x 1) => 5)

; ---- 4.2.1 -------------------------------------------------------------------

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
         ((b) 'b)) => #,(unspecified))

(check (case (car '(c d))
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

(check (or (memq 'b '(a b c)) (/ 3 0)) => (b c))

; ---- 4.2.2 -------------------------------------------------------------------

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

(check (letrec ((even? (lambda (n)
                         (if (zero? n) #t
                             (odd? (- n 1)))))
                (odd? (lambda (n)
                        (if (zero? n) #f
                            (even? (- n 1))))))
         (even? 88)) => #t)

; ---- 4.2.3 -------------------------------------------------------------------

(define x 0)

(check (begin (set! x 5)
              (+ x 1)) => 6)

(check (begin (display "4 plus 1 equals ")
              (display (+ 4 1))) => #,(unspecified))

; ---- 4.2.4 -------------------------------------------------------------------

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

; ---- 4.2.6 -------------------------------------------------------------------

(check `(list ,(+ 1 2) 4) => (list 3 4))

(check (let ((name 'a)) `(list ,name ',name)) => (list a (quote a)))

(check `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b) => (a 3 4 5 6 b))

(check `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))) => ((foo 7) . cons))

(check `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8) => #(10 5 2 4 3 8))

(check `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f) => (a `(b ,(+ 1 2) ,(foo 4 d) e) f))

(check (let ((name1 'x)
             (name2 'y))
         `(a `(b ,,name1 ,',name2 d) e)) => (a `(b ,x ,'y d) e))

; ---- 4.3.1 -------------------------------------------------------------------

(check (let-syntax ((when (syntax-rules ()
                            ((when test stmt1 stmt2 ...)
                             (if test
                                 (begin stmt1
                                        stmt2 ...))))))
         (let ((if #t))
           (when if (set! if 'now))
           if)) => now)

(check (let ((x 'outer))
         (let-syntax ((m (syntax-rules () ((m) x))))
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

; ---- 4.3.2 -------------------------------------------------------------------

(check (let ((=> #f))
         (cond (#t => 'ok))) => ok)

; ---- 5.2.1 -------------------------------------------------------------------

(define add3
  (lambda (x) (+ x 3)))

(check (add3 3) => 6)

(define first car)

(check (first '(1 2)) => 1)

; ---- 5.2.2 -------------------------------------------------------------------

(check (let ((x 5))
         (define foo (lambda (y) (bar x y)))
         (define bar (lambda (a b) (+ (* a b) a)))
         (foo (+ x 3))) => 45)

(check (let ((x 5))
         (letrec ((foo (lambda (y) (bar x y)))
                  (bar (lambda (a b) (+ (* a b) a))))
           (foo (+ x 3)))) => 45)

; ---- 6.1 ---------------------------------------------------------------------

(check (eqv? 'a 'a) => #t)

(check (eqv? 'a 'b) => #f)

(check (eqv? 2 2) => #t)

(check (eqv? '() '()) => #t)

(check (eqv? 100000000 100000000) => #t)

(check (eqv? (cons 1 2) (cons 1 2)) => #f)

(check (eqv? (lambda () 1) (lambda () 2)) => #f)

(check (eqv? #f 'nil) => #f)

(check (let ((p (lambda (x) x)))
         (eqv? p p)) => #t)

(check (eqv? "" "") => #t)

(check (eqv? '#() '#()) => #t)

(check (eqv? (lambda (x) x)
             (lambda (x) x)) => #f)

(check (eqv? (lambda (x) x)
             (lambda (y) y)) => #f)

(define gen-counter
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) n))))

(check (let ((g (gen-counter)))
         (eqv? g g)) => #t)

(check (eqv? (gen-counter) (gen-counter)) => #f)

(define gen-loser
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) 27))))

(check (let ((g (gen-loser)))
         (eqv? g g)) => #t)

(check (eqv? (gen-loser) (gen-loser)) => #f)

(check (letrec ((f (lambda () (if (eqv? f g) 'both 'f)))
                (g (lambda () (if (eqv? f g) 'both 'g))))
         (eqv? f g)) => #f)

(check (letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
                (g (lambda () (if (eqv? f g) 'g 'both))))
         (eqv? f g)) => #f)

(check (eqv? '(a) '(a)) => #t)

(check (eqv? "a" "a") => #t)

(check (eqv? '(b) (cdr '(a b))) => #t)

(check (let ((x '(a)))
         (eqv? x x)) => #t)

(check (eq? 'a 'a) => #t)

(check (eq? '(a) '(a)) => #f)

(check (eq? (list 'a) (list 'a)) => #f)

(check (eq? "a" "a") => #f)

(check (eq? "" "") => #f)

(check (eq? '() '()) => #t)

(check (eq? 2 2) => #f)

(check (eq? #\A #\A) => #f)

(check (eq? car car) => #t)

(check (let ((n (+ 2 3)))
         (eq? n n)) => #t)

(check (let ((x '(a)))
         (eq? x x)) => #t)

(check (let ((x '#()))
         (eq? x x)) => #t)

(check (let ((p (lambda (x) x)))
         (eq? p p)) => #t)

(check (equal? 'a 'a) => #t)

(check (equal? '(a) '(a)) => #t)

(check (equal? '(a (b) c) '(a (b) c)) => #t)

(check (equal? "abc" "abc") => #t)

(check (equal? 2 2) => #t)

(check (equal? (make-vector 5 'a) (make-vector 5 'a)) => #t)

(check (equal? (lambda (x) x) (lambda (y) y)) => #f)

; ---- 6.2.5 -------------------------------------------------------------------

(check (complex? 3+4i) => #t)

(check (complex? 3) => #t)

(check (real? 3) => #t)

(check (real? -2.5+0.0i) => #t)

(check (real? #e1e10) => #t)

(check (rational? 6/10) => #t)

(check (rational? 6/3) => #t)

(check (integer? 3+0i) => #t)

(check (integer? 3.0) => #t)

(check (integer? 8/4) => #t)

(check (max 3 4) => 4)

(check (max 3.9 4) => 4.0)

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

(check (modulo 13 4) => 1)

(check (remainder 13 4) => 1)

(check (modulo -13 4) => 3)

(check (remainder -13 4) => -1)

(check (modulo 13 -4) => -3)

(check (remainder 13 -4) => 1)

(check (modulo -13 -4) => -1)

(check (remainder -13 -4) => -1)

(check (remainder -13 -4.0) => -1.0)

(check (gcd 32 -36) => 4)

(check (gcd) => 0)

(check (lcm 32 -36) => 288)

(check (lcm 32.0 -36) => 288.0)

(check (lcm) => 1)

(check (numerator (/ 6 4)) => 3)

(check (denominator (/ 6 4)) => 2)

(check (denominator (exact->inexact (/ 6 4))) => 2.0)

(check (floor -4.3) => -5.0)

(check (ceiling -4.3) => -4.0)

(check (truncate -4.3) => -4.0)

(check (round -4.3) => -4.0)

(check (floor 3.5) => 3.0)

(check (ceiling 3.5) => 4.0)

(check (truncate 3.5) => 3.0)

(check (round 3.5) => 4.0)

(check (round 7/2) => 4)

(check (round 7) => 7)

; ---- 6.2.6 -------------------------------------------------------------------

(check (string->number "100") => 100)

(check (string->number "100" 16) => 256)

(check (string->number "1e2") => 100.0)

; (check (string->number "15##") => 1500.0) ; ERROR

; ---- 6.3.1 -------------------------------------------------------------------

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

; ---- 6.3.2 -------------------------------------------------------------------

(define x (list 'a 'b 'c))

(define y x)

(check y => (a b c))

(check (list? y) => #t)

(set-cdr! x 4)

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

(check (cdr '((a) b c d)) => (b c d))

(check (cdr '(1 . 2)) => 2)

(define (f) (list 'not-a-constant-list))

(define (g) '(constant-list))

(check (set-car! (f) 3) => 3)

(check (set-car! (g) 3) => 3)

(check (list? '(a b c)) => #t)

(check (list? '()) => #t)

(check (list? '(a . b)) => #f)

(check (let ((x (list 'a)))
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

(check (reverse '(a (b c) d (e (f)))) => ((e (f)) d (b c) a))

(define list-tail
  (lambda (x k)
    (if (zero? k)
        x
        (list-tail (cdr x) (- k 1)))))

(check (list-ref '(a b c d) 2) => c)

(check (list-ref '(a b c d) (inexact->exact (round 1.8))) => c)

(check (memq 'a '(a b c)) => (a b c))

(check (memq 'b '(a b c)) => (b c))

(check (memq 'a '(b c d)) => #f)

(check (memq (list 'a) '(b (a) c)) => #f)

(check (member (list 'a) '(b (a) c)) => ((a) c))

(check (memq 101 '(100 101 102)) => #f)

(check (memv 101 '(100 101 102)) => (101 102))

(define e '((a 1) (b 2) (c 3)))

(check (assq 'a e) => (a 1))

(check (assq 'b e) => (b 2))

(check (assq 'd e) => #f)

(check (assq (list 'a) '(((a)) ((b)) ((c)))) => #f)

(check (assoc (list 'a) '(((a)) ((b)) ((c)))) => ((a)))

(check (assq 5 '((2 3) (5 7) (11 13))) => #f)

(check (assv 5 '((2 3) (5 7) (11 13))) => (5 7))

; ---- 6.3.3 -------------------------------------------------------------------

(check (symbol? 'foo) => #t)

(check (symbol? (car '(a b))) => #t)

(check (symbol? "bar") => #f)

(check (symbol? 'nil) => #t)

(check (symbol? '()) => #f)

(check (symbol? #f) => #f)

(check (symbol->string 'flying-fish) => "flying-fish")

(check (symbol->string 'Martin) => "Martin") ; incompatible

(check (symbol->string (string->symbol "Malvina")) => "Malvina")

(check (eq? 'mISSISSIppi 'mississippi) => #f) ; incompatible

(check (string->symbol "mISSISSIppi") => mISSISSIppi)

(check (eq? 'bitBlt (string->symbol "bitBlt")) => #t) ; incompatible

(check (eq? 'JollyWog (string->symbol (symbol->string 'JollyWog))) => #t)

(check (string=? "K. Harper, M.D." (symbol->string (string->symbol "K. Harper, M.D."))) => #t)

; ---- 6.3.4 -------------------------------------------------------------------

(check (char? #\a) => #t)

(check (char? #\A) => #t)

(check (char? #\() => #t)

(check (char? #\ ) => #t)

(check (char? #\space) => #t)

(check (char? #\newline) => #t)

; ---- 6.3.5 -------------------------------------------------------------------

(check (string? "The word \"recursion\" has many meanings.") => #t)

(define (f) (make-string 3 #\*))

(define (g) "***")

(string-set! (f) 0 #\?)

(string-set! (g) 0 #\?)

(string-set! (symbol->string 'immutable) 0 #\?)

; ---- 6.3.6 -------------------------------------------------------------------

(check (vector? #(0 (2 2 2 2) "Anna")) => #t)

(check (vector 'a 'b 'c) => #(a b c))

(check (vector-ref '#(1 1 2 3 5 8 13 21) 5) => 8)

(check (vector-ref '#(1 1 2 3 5 8 13 21)
                   (let ((i (round (* 2 (acos -1)))))
                     (if (inexact? i)
                         (inexact->exact i)
                         i))) => 13)

(check (let ((vec (vector 0 '(2 2 2 2) "Anna")))
         (vector-set! vec 1 '("Sue" "Sue"))
         vec) => #(0 ("Sue" "Sue") "Anna"))

(vector-set! '#(0 1 2) 1 "doe")

(check (vector->list '#(dah dah didah)) => (dah dah didah))

(check (list->vector '(dididit dah)) => #(dididit dah))

; ---- 6.4 ---------------------------------------------------------------------

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

(check (map + '(1 2 3) '(4 5 6)) => (5 7 9))

(check (let ((count 0))
         (map (lambda (ignored)
                (set! count (+ count 1))
                count)
              '(a b))) => (1 2))

(check (let ((v (make-vector 5)))
         (for-each (lambda (i)
                     (vector-set! v i (* i i)))
                   '(0 1 2 3 4))
         v) => #(0 1 4 9 16))

(check (force (delay (+ 1 2))) => 3)

(check (let ((p (delay (+ 1 2))))
         (list (force p) (force p))) => (3 3))

(define a-stream
  (letrec ((next (lambda (n)
                   (cons n (delay (next (+ n 1)))))))
    (next 0)))

(define head car)

(define tail
  (lambda (stream) (force (cdr stream))))

(check (head (tail (tail a-stream))) => 2)

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

(check (eqv? (delay 1) 1) => #f)

(check (pair? (delay (cons 1 2))) => #t)

(check (call-with-current-continuation
         (lambda (exit)
           (for-each (lambda (x)
                       (if (negative? x)
                           (exit x)))
                     '(52 0 37 -3 245 19))
           #t)) => -3)

(define list-length
  (lambda (obj)
    (call-with-current-continuation
      (lambda (return)
        (letrec ((r (lambda (obj)
                      (cond ((null? obj) 0)
                            ((pair? obj)
                             (+ (r (cdr obj)) 1))
                            (else (return #f))))))
          (r obj))))))

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

; ---- 6.5 ---------------------------------------------------------------------

; (check (eval '(* 7 3) (scheme-report-environment 5)) => 21) ; ERROR

; (check (let ((f (eval '(lambda (f x) (f x x)) (null-environment 5))))
;          (f + 10)) => 20) ; ERROR

; ---- EXAMPLE -----------------------------------------------------------------

(define integrate-system
  (lambda (system-derivative initial-state h)
    (let ((next (runge-kutta-4 system-derivative h)))
      (letrec ((states
                 (cons initial-state
                       (delay (map-streams next
                                           states)))))
        states))))

(define runge-kutta-4
  (lambda (f h)
    (let ((*h (scale-vector h))
          (*2 (scale-vector 2))
          (*1/2 (scale-vector (/ 1 2)))
          (*1/6 (scale-vector (/ 1 6))))
      (lambda (y)
        ;; y is a system state
        (let* ((k0 (*h (f y)))
               (k1 (*h (f (add-vectors y (*1/2 k0)))))
               (k2 (*h (f (add-vectors y (*1/2 k1)))))
               (k3 (*h (f (add-vectors y k2)))))
          (add-vectors y
                       (*1/6 (add-vectors k0
                                          (*2 k1)
                                          (*2 k2)
                                          k3))))))))

(define elementwise
  (lambda (f)
    (lambda vectors
      (generate-vector
        (vector-length (car vectors))
        (lambda (i)
          (apply f
                 (map (lambda (v) (vector-ref v i))
                                    vectors)))))))
(define generate-vector
  (lambda (size proc)
    (let ((ans (make-vector size)))
      (letrec ((loop (lambda (i)
                       (cond ((= i size) ans)
                             (else (vector-set! ans i (proc i))
                                   (loop (+ i 1)))))))
        (loop 0)))))

(define add-vectors (elementwise +))

(define scale-vector
  (lambda (s)
    (elementwise (lambda (x) (* x s)))))

(define map-streams
  (lambda (f s)
    (cons (f (head s))
          (delay (map-streams f (tail s))))))

(define head car)

(define tail
  (lambda (stream) (force (cdr stream))))

(define damped-oscillator
  (lambda (R L C)
    (lambda (state)
      (let ((Vc (vector-ref state 0))
            (Il (vector-ref state 1)))
        (vector (- 0 (+ (/ Vc (* R C)) (/ Il C)))
                (/ Vc L))))))

(define the-states
  (integrate-system
    (damped-oscillator 10000 1000 .001)
    '#(1 0)
    .01))

; ------------------------------------------------------------------------------

(check-report)

(exit (check-passed? check:correct))
