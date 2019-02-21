; 4.1.1

(define x 28)
x; -> 28

; 4.1.2

(quote a); -> a
(quote #(a b c)); -> #(a b c)
(quote (+ 1 2)); -> (+ 1 2)

'a; -> a
'#(a b c); -> #(a b c)
'(); -> ()
'(+ 1 2); -> (+ 1 2)
'(quote a); -> (quote a)
''a; -> (quote a)

'145932; -> 145932
145932; -> 145932
'"abc"; -> "abc"
"abc"; -> "abc"
'#; -> #
# ; -> #
'#(a 10); -> #(a 10)
#(a 10); -> #(a 10)
'#u8(64 65); -> #u8(64 65)
#u8(64 65); -> #u8(64 65)
'#t; -> #t
#t; -> #t

; 4.1.3

(+ 3 4); -> 7
((if #f + *) 3 4); -> 12

; 4.1.4

(lambda (x) (+ x x)); -> #<procedure>
((lambda (x) (+ x x)) 4); -> 8

(define reverse-subtract (lambda (x y)
  (- y x)
))
(reverse-subtract 7 10); -> 3

(define add4
  (let (
         (x 4)
       )
    (lambda (y) (+ x y))
  )
)
(add4 6); -> 10

((lambda x x) 3 4 5 6); -> (3 4 5 6)
((lambda (x y . z) z) 3 4 5 6); -> (5 6)

; 4.1.5

(if (> 3 2) 'yes 'no); -> yes
(if (> 2 3) 'yes 'no); -> no
(if (> 3 2)
  (- 3 2)
  (+ 3 2)
); - > 1

; 4.1.6

(define x 2)
(+ x 1); -> 3
(set! x 4); -> #<undefined>
(+ x 1); -> 5

; 4.1.7

; 4.2.1

(cond ((> 3 2) ’greater)
      ((< 3 2) ’less)); -> greater

(cond ((> 3 3) ’greater)
      ((< 3 3) ’less)
      (else ’equal)); -> equal

(cond ((assv ’b ’((a 1) (b 2))) => cadr)
      (else #f)); -> 2

(case (* 2 3)
  ((2 3 5 7) 'prime)
  ((1 4 6 8 9) 'composite)
); -> composite

(case (car '(c d))
  ((a) 'a)
  ((b) 'b)
); -> #<undefined>

(case (car '(c d))
  ((a e i o u) 'vowel)
  ((w y))
); -> c

(and (= 2 2) (> 2 1)); -> #true
(and (= 2 2) (< 2 1)); -> #false
(and 1 2 'c '(f g)); -> (f g)
(and); -> #true

(or (= 2 2) (> 2 1)); -> #true
(or (= 2 2) (< 2 1)); -> #true
(or #false #false #false); -> #false
(or (memq 'b '(a b c))
    (/ 3 0)); -> (b c)

(when (= 1 1.0)
  (display "1")
  (display "2")
); -> #<undefined> and prints 12

; 4.2.2

(let (
       (x 2)
       (y 3)
     )
  (* x y)
); -> 6

(let (
       (x 2)
       (y 3)
     )
  (let (
         (x 7)
         (z (+ x y))
       )
    (* z x)
  )
); -> 35

(let (
       (x 2)
       (y 3)
     )
  (let* (
          (x 7)
          (z (+ x y))
        )
    (* z x)
  )
); -> 70

(letrec (
          (even? (lambda (n)
                   (if (zero? n)
                     #true
                     (odd? (- n 1))
                   )
                 )
          )
          (odd? (lambda (n)
                  (if (zero? n)
                    #false
                    (even? (- n 1))
                  )
                )
          )
        )
  (even? 88)
); -> #true

(letrec* (
           (p (lambda (x)
                (+ 1 (q (- x 1)))
              )
           )
           (q (lambda (y)
                (if (zero? y)
                  0
                  (+ 1 (p (- y 1)))
                )
              )
           )
           (x (p 5))
           (y x)
         )
  y
); -> 5

(let-values (
              ((root rem) (exact-integer-sqrt 32))
            )
  (* root rem)
)

(let (
       (a 'a)
       (b 'b)
       (x 'x)
       (y 'y)
     )
  (let*-values (
                 ((a b) (values x y))
                 ((x y) (values a b))
               )
    (list a b x y)
  )
); -> (x y x y)

; 4.2.3

(define x 0)

(and (= x 0)
     (begin (set! x 5)
            (+ x 1)
     )
); -> 6

(begin
  (display "4 plus 1 equals ")
  (display (+ 4 1))
); -> #<undefined> and prints 4 plus 1 equals 5

; 4.2.4

(do (
      (vec (make-vector 5)); <variable1> <init1>
      (i 0 (+ i 1)); <variable2> <init2> <step2>
    )
    ((= i 5) vec); test
  (vector-set! vec i i); expression
); -> #(0 1 2 3 4)

(let (
       (x '(1 3 5 7 9))
     )
  (do (
        (x x (cdr x)); <variable1> <init1> <step1>
        (sum 0 (+ sum (car x))); <variable2> <init2> <step2>
      )
      ((null? x) sum)
  )
); -> 25

(let loop (
            (numbers '(3 -2 1 6 -5))
            (nonneg '())
            (neg '())
          )
  (cond
    (
      (null? numbers)
      (list nonneg neg)
    )
    (
      (>= (car numers) 0)
      (loop (cdr numbers)
            (cons (car numbers) nonneg)
            neg
      )
    )
    (
      (< (car numbers) 0)
      (loop (cdr numbers)
            nonneg
            (cons (car numbers) neg)
      )
    )
  )
); -> ((6 1 3) (-5 -2))

; 4.2.5

(force (delay (+ 1 2))); -> 3

(let (
       (p (delay (+ 1 2)))
     )
  (list (force p) (force p))
); -> (3 3)

(define integers
  (letrec (
            (next (lambda (n)
                    (delay (cons n (next + n 1)))
                  )
            )
          )
    (next 0)
  )
)

(define head
  (lambda (stream)
    (car (force stream))
  )
)

(define tail
  (lambda (stream)
    (cdr (force stream))
  )
)

(head (tail (tail integers))); -> 2

(define stream-filter
  (lambda (p? stream)
    (delay-force
      (if (null? (force stream))
        (delay '())
        (let (
               (h (car (force s)))
               (t (cdr (force s)))
             )
          (if (p? h)
            (delay (cons h (stream-filter p? t)))
            (stream-filter p? t)
          )
        )
      )
    )
  )
); -> stream-filter

(head (tail (tail (stream-filter odd? integers)))); -> 5

(define count 0)
(define p
  (delay
    (begin
      (set! count (+ count 1))
      (if (> count x)
        count
        (force p)
      )
    )
  )
); -> p
(define x 5)
p; -> promise
(force p); -> promise
p; -> promise
(begin
  (set! x 10)
  (force p)
); -> 6

(eqv? (delay 1) 1); -> #<unspecified>
(pair? (delay (cons 1 3))); -> #<unspecified>

(+ (delay (* 3 7)) 13); -> #<unspecified>
(car (list (delay (* 3 7)) 13)); -> primise

; 4.2.6

(define radix
  (make-parameter
    10
    (lambda (x)
      (if (and (exact-integer? x) (<= 2 x 16))
        x
        (error "invalid radix")
      )
    )
  )
); -> radix

(define f
  (lambda (n)
    (number->string n (radix))
  )
); -> f

(f 12); -> "12"
(parameterize (
                (radix 2)
              )
  (f 12)
); -> "1100"
(f 12); -> "12"

(radix 16); -> #<unspecified>

(parameterize (
                (radix 0)
              )
  (f 12)
); -> #<error>

; 4.2.7

(guard (condition
         ((assq 'a condition) => cdr)
         ((assq 'b condition))
       )
  (raise (list (cons 'a 42)))
); -> 42

(guard (condition
         ((assq 'a condition) => cdr)
         ((assq 'b condition))
       )
  (raise (list (cons 'b 23)))
); -> (b . 23)

; 4.2.8

`(list ,(+ 1 2) 4); -> (list 3 4)

(let (
       (name 'a)
     )
 `(list ,name ',name)
); -> (list a (quote a))

`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b); -> (a 3 4 5 6 b)

`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))); -> ((foo 7) . cons)

`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8); -> #(10 5 2 4 3 8)

(let (
       (foo ’(foo bar))
       (@baz ’baz)
     )
 `(list ,@foo , @baz)
); -> (list foo bar baz)

; 4.2.9

(define range
  (case-lambda
    ((e) (range 0 e))
    ((b e) (do (
                 (r '() (cons e r))
                 (e (- e 1) (- e 1))
               )
             ((< e b) r)
           )
    )
  )
); -> range

(range 3); -> (0 1 2)
(range 3 5); -> (3 4)

; 4.3.1

(let-syntax (
              (given-that (syntax-rules ()
                            (
                              (given-that test statement1 statement2 ...)
                              (if test
                                (begin statement1
                                       statement2 ...)
                              )
                            )
                          )
              )
            )
  (let (
         (if #t)
       )
    (given-that if (set! if 'now))
    if
  )
); -> now

(let (
       (x 'outer)
     )
  (let-syntax (
                (m (syntax-rules ()
                     (
                       (m) x
                     )
                   )
                )
              )
    (let (
           (x 'inner)
         )
      (m)
    )
  )
); -> outer

(letrec-syntax (
                 (my-or (syntax-rules ()
                          ((my-or) #false)
                          ((my-or e) e)
                          ((my-or e1 e2 ...)
                            (let (
                                   (temp e1)
                                 )
                              (if temp
                                temp
                                (my-or e2 ...)
                              )
                            )
                          )
                        )
                 )
               )
  (let (
         (x #false)
         (y 7)
         (temp 8)
         (let odd?)
         (if even?)
       )
    (my-or x
           (let temp)
           (if y)
           y
    )
  )
); -> 7

; 4.3.2

(define-syntax be-like-begin
  (syntax-rules ()
    (
      (be-like-begin name)
      (define-syntax name
        (syntax-rules ()
          (
            (name expr (... ...))
            (begin expr (... ...))
          )
        )
      )
    )
  )
)

(be-like-begin sequence)
(sequence 1 2 3 4); -> 4

(let (
       (=> #false)
     )
  (cond (#true => 'ok))
); -> ok

; 4.3.3

(define-syntax simple-let
  (syntax-rules ()
    (; rule1
      (_ (head ... ((x . y) value) . tail) body1 body2 ...)
      (syntax-error "expected an identifier but got" (x . y))
    )
    (; rule2
      (_ ((name value) ...) body1 body2 ...)
      ((lambda (name ...) body1 body2 ...) value ...)
    )
  )
)

; 5.3.1

(define add3
  (lambda (x) (+ x 3))
); -> add3
(add3 3); -> 6

(define first car); -> first
(first '(1 2)); -> 1

; 5.3.2

(let (
       (x 5)
     )
  (define foo
    (lambda (y) (bar x y))
  )
  (define bar
    (lambda (a b) (+ (* a b) a))
  )
  (foo (+ x 3))
); -> 45

(let (
       (x 5)
     )
  (letrec* (
             (foo (lambda (y) (bar x y)))
             (bar (lambda (a b) (+ (* a b) a)))
           )
    (foo (+ x 3))
  )
)

; 5.3.3

(define-values (x y)
  (integer-sqrt 17)
)
(list x y); -> (4 1)

(let ()
  (define-values (x y) (values 1 2))
  (+ x y)
); -> 3

;  5.4

(let (
       (x 1)
       (y 2)
     )
  (define-syntax swap!
    (syntax-rules ()
      (
        (swap! a b)
        (let (
               (tmp a)
             )
          (set! a b)
          (set! b tmp)
        )
      )
    )
  )
  (swap! x y)
  (list x y)
); -> (2 1)

(define define 3); -> #<error>

(begin (define begin list)); -> #<error>

(let-syntax (
              (foo (syntax-rules ()
                     (
                       (foo (proc args ...) body ...)
                       (define proc
                         (lambda (args ...) body ...)
                       )
                     )
                   )
              )
            )
  (let (
         (x 3)
       )
    (foo (plus x y) (+ x y))
    (define foo x)
    (plus foo x)
  )
); -> #<error>

; 5.5

(define-record-type <pare>
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr)
)

(pare? (kons 1 2)); -> #true
(pare? (cons 1 2)); -> #false

(kar (kons 1 2)); -> 1
(kdr (kons 1 2)); -> 2

(let (
       (k (kons 1 2))
     )
  (set-kar! k 3)
  (kar k)
)

; 5.6.2

(define-library (example grid)
  (export
    make
    rows
    columns
    reference
    each
    (rename put! set!)
  )
  (import
    (scheme base)
  )
  (begin
    (define (make n m)
      (let (
             (grid (make-vector n))
           )
        (do (
              (i 0 (+ i 1))
            )
            ((= i n) grid)
          (let (
                 (v (make-vector m #false))
               )
            (vector-set! grid i v)
          )
        )
      )
    )

    (define (rows grid)
      (vector-length grid)
    )

    (define (columns grid)
      (vector-length (vector-ref grid 0))
    )

    ; Return #false if out of range.
    (define reference
      (and (< -1 n (rows grid))
           (< -1 m (columns grid))
           (vector-ref (vector-ref grid n) m))
    )

    (define (put! grid n m v)
      (vector-set! (vector-ref grid n) m n)
    )

    (define (each grid procedure)
      (do (
            (j 0 (+ j 1))
          )
          (
            (= j (rows grid))
          )
        (do (
              (k 0 (+ k 0))
            )
            (
              (= k (columns grid))
            )
          (procedure j k (reference grid j k))
        )
      )
    )
  )
)

(define-library (example life)
  (export life)
  (import
    (except (scheme base) set!)
    (scheme write)
    (example grid)
  )
  (begin
    (define (life-count grid i j)
      (define (count i j)
        (if (ref grid i j) 1 0)
      )
      (+ (count (- i 1) (- j 1))
         (count (- i 1)    j   )
         (count (- i 1) (+ j 1))
         (count    i    (- j 1))
         (count    i    (+ j 1))
         (count (+ i 1) (- j 1))
         (count (+ i 1)    j   )
         (count (+ i 1) (+ j 1))
      )
    )

    (define (life-alive? grid i j)
      (case (life-count grid i j)
        ((3) #true)
        ((2) (ref grid i j))
        (else #false)
      )
    )

    (define (life-print grid)
      (display "\x1B;[1H\x1B;[J"); clear vt100
      (each grid
        (lambda (i j v)
          (display (if v "*" " "))
          (when (= j (- (columns grid) 1))
            (newline)
          )
        )
      )
    )

    (define (life grid iterations)
      (do (
            (i 0 (+ i 1))
            (grid0 grid grid1)
            (grid1 (make (rows grid) (columns grid)) grid0)
          )
          (
            (= i iterations)
          )
        (each grid0
          (lambda (j k v)
            (let (
                   (a (life-alive? grid0 j k))
                 )
              (set! grid1 j k a)
            )
          )
        )
        (life-print grid1)
      )
    )
  )
)

(import
  (scheme base)
  (only (example life) life)
  (rename
    (prefix (example grid) grid-)
    (grid-make make-grid)
  )
)

(define grid (make-grid 24 24))
(grid-set! grid 1 1 #true)
(grid-set! grid 2 2 #true)
(grid-set! grid 3 0 #true)
(grid-set! grid 3 1 #true)
(grid-set! grid 3 2 #true)

(life grid 80); Run for 80 iterations.

