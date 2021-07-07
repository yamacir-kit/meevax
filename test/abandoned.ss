(define p1 (make-parameter 1))
(define p2 (make-parameter "hoge"))

(check (p1)   => 1)
(check (p1 2) => 2)
(check (p1)   => 2)

(parameterize ((p1 42)
               (p2 "fuga"))
  (print "p1 = " (p1)) (newline)
  (print "p2 = " (p2)) (newline)
  (check (p1) => 42)
  (check (p2) => "fuga")
  (list (p1) (p2)))


(check (string=? (make-string 3) "") => #f)
(check (string=? (make-string 3) "   ") => #t)
(check (string=? (make-string 3 #\a) "aaa") => #t)

(check (string=? (string) "") => #t)
(check (string=? (string #\h #\o #\g #\e) "hoge") => #t)

(check (string-length "") => 0)
(check (string-length "abc") => 3)

(check (string-ref "abc" 1) => #\b)

(define s "abc")
(string-set! s 1 #\x)
(check s => "axc")

(check (string=?  "abc" "abc") => #t)
(check (string<?  "abc" "bcd") => #t)
(check (string>?  "bcd" "abc") => #t)
(check (string<=? "abc" "abd") => #t)
(check (string>=? "abc" "aba") => #t)

(check (string-ci=?  "aBc" "AbC") => #t)
(check (string-ci<?  "aBc" "BcD") => #t)
(check (string-ci>?  "bCd" "AbC") => #t)
(check (string-ci<=? "aBc" "AbD") => #t)
(check (string-ci>=? "aBc" "AbA") => #t)

; ---- string-foldcase ---------------------------------------------------------

(check (string-upcase   "AbdEgH") => "ABDEGH")
(check (string-downcase "AbdEgH") => "abdegh")

; ---- string-copy (substring) -------------------------------------------------

(check (string-copy "abcde")     => "abcde")
(check (string-copy "abcde" 1)   =>  "bcde")
(check (string-copy "abcde" 1 4) =>  "bcd" )

; ---- string-append -----------------------------------------------------------

(check (string-append)                   => "")
(check (string-append "abc")             => "abc")
(check (string-append "abc" "def")       => "abcdef")
(check (string-append "abc" "def" "ghi") => "abcdefghi")

; ---- string->list ------------------------------------------------------------

(check (string->list "abcde")     => (#\a #\b #\c #\d #\e))
(check (string->list "abcde" 1)   => (    #\b #\c #\d #\e))
(check (string->list "abcde" 1 4) => (    #\b #\c #\d    ))

; ---- string-fill! ------------------------------------------------------------

(let ((s "abcde")) (check (begin (string-fill! s #\x) s) => "xxxxx"))
(let ((s "abcde")) (check (begin (string-fill! s #\x 1) s) => "axxxx"))
(let ((s "abcde")) (check (begin (string-fill! s #\x 1 4) s) => "axxxe"))


(define swap!
  (fork/csc
    (lambda (swap! x y)
      (set-debug! #t)
      (let ((temporary (string->symbol)))
       `(,let ((,temporary ,x))
          (,set! ,x ,y)
          (,set! ,y ,temporary)) ))))

(define swap!
  (fork/csc
    (lambda (swap! x y)
     `(,let ((,value ,x))
        (,set! ,x ,y)
        (,set! ,y ,value)) )))

(define-syntax (swap! x y)
 `(,let ((,value ,x))
    (,set! ,x ,y)
    (,set! ,y ,value)))

; (define swap!
;   (er-macro-transformer
;     (lambda (expression rename compare)
;       (let ((a (cadr expression))
;             (b (caddr expression)))
;        `(,(rename 'let) ((,(rename 'value) ,a))
;           (,(rename 'set!) ,a ,b)
;           (,(rename 'set!) ,b ,(rename 'value))) ))))

(define loop
  (fork/csc
    (lambda form
     `(,call/cc
        (,lambda (exit)
          (,let ,rec ()
           ,(cadr form)
            (,rec)))) )))

(define f
  (lambda ()
    (define x 0)

    (define let     3.14)
    (define call/cc 3.141)
    ; (define lambda  3.1415) ; IMPLEMENTATION MISS
    (define exit    3.14159)
    (define rec     3.141592)

    (loop
      (if (< 9 x)
          (begin (display "!")
                 (display exit)
                 (exit 42))
          (begin (display x)
                 (set! x (+ x 1)) )))))

; (define-syntax loop
;   (non-hygienic-macro-transformer
;     (lambda (form)
;      `(call-with-current-continuation
;         (lambda (exit)
;           (let loop ()
;             ,form
;             (loop)))))))

; (define-syntax loop
;   (sc-macro-transformer
;     (lambda (form environment)
;      `(call-with-current-continuation
;         (lambda (exit)
;           (let loop ()
;            ,(make-syntactic-closure environment '(exit) (cadr form))
;             (loop)))))))

; ------------------------------------------------------------------------------
;  Library
; ------------------------------------------------------------------------------

(define define-something
  (fork/csc
    (lambda (_ name value)
     `(,define ,name ,value))))

(define define-library
  (fork/csc
    (lambda (define-library name . declarations)
     `(,define ,name
        (,fork/csc
          (,lambda (,this . ,expression)
            ; (,begin (,define ,name ,this))
            ,@declarations
            ; (,if (,null? ,expression) ,this
            ;      (,begin
            ;        (,display "; library\t; received expression " ,expression "\n")
            ;        (,display ";\t\t; evaluate " (,car ,expression) " (a.k.a rename)\n")
            ;        (evaluate (,car ,expression))
            ;        )
            ;      )
            )))
     )))

; (define export
;   (fork/csc
;     (lambda (_ . export-specs)
;      `(,display "; dummy-export\t; " ',export-specs "\n"))))

; (define export
;   (fork/csc
;     (lambda (this . export-specs)
;      `(stage ,@(map (lambda (each)
;                       (list quote each))
;                     export-specs)))))

; (define import
;   (fork/csc
;     (lambda (import . import-set)
;      `(,display "; dummy-import\t; " ',import-set "\n"))))

; (define instantiate-library
;   (fork/csc
;     (lambda (this library-name)
;      `(,let ((,object (,reference ,library-name)))
;         (,object)))))

; TODO REMOVE
; (define evaluate-in
;   (fork/csc
;     (lambda (this namespace identifier)
;      `((,reference ,namespace) ',identifier))))

(define-library (example empty) '())

(define-library (scheme base)
  (export x)
  (begin
    (define x 42)
    (display "instantiating dummy-library (scheme base)!\n")))

(define-library (example hello)
  (export hello
          goodbye)

  (begin
    (define hello
      (lambda ()
        (begin (display "hello, world!")
               (newline) )))

    (define goodbye
      (lambda ()
        (begin (display "goodbye, world!")
               (newline) )))

    (define greet-to
      (lambda (name)
        (begin (display "hello, " name)
               (newline) ))))

  (begin
    (define one   1)
    (define two   2)
    (define three 3) ))

; XXX this cause compile error (bug)
; (lambda ()
;   (begin
;     (define x 1)
;     (define y 2))
;   (+ x y)
;   )

; (define-library (example grid)
;   (export make
;           rows
;           columns
;           reference
;           each
;           (rename put! set!))
;
;   (import (scheme base))
;
;   (begin
;
;     (define make
;       (lambda (n m)
;         (let ((grid (make-vector n)))
;           (do ((i 0 (+ i 1)))
;               ((= i n) grid)
;             (let ((v (make-vector m #false)))
;               (vector-set! grid i v))))))
;
;     (define rows
;       (lambda (grid)
;         (vector-length grid)))
;
;     (define columns
;       (lambda (grid)
;         (vector-length (vector-ref grid 0))))
;
;     (define reference
;       (lambda (grid n m)
;         (and (< -1 n (rows grid))
;              (< -1 m (columns grid))
;              (vector-ref (vector-ref grid n) m))))
;
;     (define put!
;       (lambda (grid n m v)
;         (vector-set!  (vector-ref grid n) m v)))
;
;     (define each
;       (lambda (grid procedure)
;         (do ((j 0 (+ j 1)))
;             ((= j (rows grid)))
;           (do ((k 0 (+ k 0)))
;               ((= k (columns grid)))
;             (procedure j k (reference grid j k))))))))
;
; (define-library (example life)
;   (export life)
;   (import (except (scheme base) set!)
;           (scheme write)
;           (example grid))
;
;   (begin
;
;     (define life-count
;       (lambda (grid i j)
;
;         (define (count i j)
;           (if (reference grid i j) 1 0))
;
;         (+ (count (- i 1) (- j 1))
;            (count (- i 1)    j   )
;            (count (- i 1) (+ j 1))
;            (count    i    (- j 1))
;            (count    i    (+ j 1))
;            (count (+ i 1) (- j 1))
;            (count (+ i 1)    j   )
;            (count (+ i 1) (+ j 1)))))
;
;     (define life-alive?
;       (lambda (grid i j)
;         (case (life-count grid i j)
;           ((3) #true)
;           ((2) (reference grid i j))
;           (else #false))))
;
;     (define life-print
;       (lambda (grid)
;         (display "\x1B;[1H\x1B;[J"); clear vt100
;         (each grid
;           (lambda (i j v)
;             (display (if v "*" " "))
;             (when (= j (- (columns grid) 1))
;               (newline))))))
;
;     (define life
;       (lambda (grid iterations)
;         (do ((i 0 (+ i 1))
;              (grid0 grid grid1)
;              (grid1 (make (rows grid)
;                           (columns grid))
;                     grid0))
;             ((= i iterations))
;           (each grid0
;             (lambda (j k v)
;               (let ((a (life-alive? grid0 j k)))
;                 (set! grid1 j k a))))
;           (life-print grid1))))))

(define-library (example value)
  (import (scheme base))
  (export increment
          reference-value)
  (begin
    (define value 0)
    (define increment
      (lambda ()
        (set! value (+ value 1))))
    (define reference-value
      (lambda () value))))

; (define reference-value
;   (lambda operands
;     (let ((evaluate (reference (example value))))
;       (evaluate `(reference-value ,@operands)))))
;
; (define increment
;   (lambda operands
;     (let ((evaluate (reference (example value))))
;       (evaluate `(increment ,@operands)))))

(define from
  (fork/csc
    (lambda (this library-name expression)
     `(,apply (,reference ,library-name) ,expression) )))

(define reference-value
  (lambda xs
    (from (example value)
         `(reference-value ,xs) )))

(define increment
  (lambda xs
    (from (example value)
         `(increment ,xs) )))

(define Module
  (fork/csc
    (lambda (this)
      (begin
        (define x 1)
        (define y 2)
        (define div
          (lambda ()
            (/ x y) ))
        (define sum
          (lambda ()
            (+ x y) ))))))

(define factory ; letrec
  (fork/csc
    (lambda (this)
      (letrec ((value 0)
               (increment
                 (lambda ()
                   (set! value (+ value 1)) ))
               (get
                 (lambda () value)))
       `(begin (define increment ,increment)
               (define get ,get)) ))))

(define factory ; internal-define
  (fork/csc
    (lambda (this)
      (define value 0)
      (define increment
        (lambda ()
          (set! value (+ value 1)) ))
      (define get
        (lambda () value))

     `(begin (define increment ,increment)
             (define get ,get)) )))

(define factory
  (fork/csc
    (lambda (this)

      (begin (define value 0)

             (define increment
               (lambda ()
                 (set! value (+ value 1)) ))

             (define get
               (lambda () value))

             ; (define even?
             ;   (lambda ()
             ;     (if (zero? value) #true
             ;         (odd? (- value 1)) )))
             ;
             ; (define odd?
             ;   (lambda ()
             ;     (if (zero? value) #false
             ;         (even? (- value 1)) )))
             )

     `(,begin (define increment ,increment)
              (define get ,get)
              ; (define even? ,even?)
              )
     )))

(define let-syntax
  (fork/csc
    (lambda (let-syntax bindings . body)
     `((fork/csc
         (,lambda (,this ,@(map car bindings))
            ,@body
            )
         )
       ,@(map cadr bindings)) )))

; (let-syntax ((given-that (fork/csc
;                            (lambda (this test . statements)
;                             `(,if test
;                                  (,begin ,statements))))))
;   (let ((if #true))
;     (given-that if (set! if 'now))
;     if))

; ((fork/csc
;    (lambda (this given-that)
;      (let ((if #true))
;        (given-that if (set! if 'now))
;        if)
;      )
;    )
;  (fork/csc
;    (lambda (this test . statements)
;     `(,if test
;        (,begin ,statements)))))

; (let ((x 'outer))
;   (let-syntax ((m (fork/csc
;                     (lambda (m) x))))
;     (let ((x 'inner))
;       (m))))

(let ((x 'outer))
  (fork/csc
    (lambda (this)
      (begin (define m (fork/csc
                         (lambda (this) x)) ))
      (let ((x 'inner))
        (m) ))))

; (define letrec* ; transform to internal-definitions
;   (fork/csc
;     (lambda (letrec* bindings . body)
;       ((lambda (definitions)
;         `((,lambda () ,@definitions ,@body)))
;        (map (lambda (x) (cons 'define x)) bindings)))))

; (define letrec-syntax
;   (fork/csc
;     (lambda (letrec-syntax bindings . body)
;       ((lambda (definitions)
;         `((,lambda ()
;             ,@definitions
;             ,@body)))
;        (map (lambda (x) (cons define x)) bindings)))))
;
;
; ; (define letrec-syntax let-syntax)
;
; (letrec-syntax ((or (fork/csc
;                       (lambda (or . tests)
;                         (cond
;                           ((null? tests) #false)
;                           ((null? (cdr tests)) (car tests))
;                           (#true ; else
;                             (list (list lambda (list result)
;                                         (list if result
;                                               result
;                                               (cons or (cdr tests))))
;                                   (car tests))))))))
;   (let ((x #false)
;         (y 7)
;         (temp 8)
;         (let odd?)
;         (if even?))
;     (or x
;         (let temp)
;         (if y)
;         y)))

(define scheme
  (fork/csc
    (lambda (this . submodule)

      (begin (define identity
               (lambda (x) x) )

             (define unspecified
               (lambda ()
                 (if #false #false) ))
             ) ; begin

      (begin (define println
               (fork/csc
                 (lambda (this . xs)
                  `(,display ,@xs "\n"))))
        )

      (begin (define base
               (fork/csc
                 (lambda (this)

                   (begin (define null-environment
                            (lambda () this) )

                          (define eq?
                            (procedure equivalence.so "equals") )

                          (define eqv?
                            (procedure equivalence.so "equivalent") )

                          ; (define hello
                          ;   (lambda ()
                          ;     (display "hello, world!\n") ))
                          (define hello
                            (lambda ()
                              (println "hello, world!")))
                          )

                  `(,begin (,define hello, hello))

                   ))) ; base
             ) ; begin

      (cond
        ((null? submodule) this)
        ((null? (car submodule)) this)
        ((eq? (car submodule) 'base) `(,base))
        (else (error))
        )
     ))) ; scheme

; (define let-syntax
;   (fork/csc
;     (lambda (let-syntax bindings . body)
;
;       (let ((definitions (map (lambda (x) (cons define x)) bindings)))
;        `(,fork/csc
;           (,lambda (this)
;             (,begin ,definitions)
;             ,@body
;             ))
;         )
;       )))
;
; (let-syntax ((given-that (fork/csc
;                            (lambda (this test . statements)
;                             `(,if test
;                                  (,begin ,statements))))))
;   (let ((if #true))
;     (given-that if (set! if 'now))
;     if))
;
; (let ((x 'outer))
;   (let-syntax ((m (fork/csc
;                     (lambda (m) x))))
;     (let ((x 'inner))
;       (m))))
