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

