(import (meevax experimental) ; disassemble
        (scheme base)
        (scheme write)
        (scheme process-context)
        (srfi 78)
        )

(define (object->string x)
  (parameterize ((current-output-port (open-output-string "")))
    (write x)
    (get-output-string (current-output-port))))

; ------------------------------------------------------------------------------

(define (f1)
  (car '(a b)))

(check (object->string (car f1))
  => "(load-constant ((a b)) \
       load-absolute #,(identity car) \
       tail-call)")

; ------------------------------------------------------------------------------

(define (f2)
  ((lambda (x)
     (+ x 1))
   42))

(check (object->string (car f2))
  => "(load-constant (42) \
       load-closure (load-constant (1) \
                     load-relative #,(identity x) \
                     cons \
                     load-absolute #,(identity +) \
                     tail-call) \
       tail-call)")

; ------------------------------------------------------------------------------

(define (f3)
  (let ((x 42))
    (+ x 1)))

(check (object->string (car f3))
  => "(load-constant (42) \
       load-closure (load-constant (1) \
                     load-relative #,(identity x) \
                     cons \
                     load-absolute #,(identity +) \
                     tail-call) \
       tail-call)")

; ------------------------------------------------------------------------------

(define (f)
  (define x 1)
  (define y 2)
  (+ x y))

(check (object->string (car f))
  => "(load-constant (() ()) \
       load-closure (load-constant 1 \
                     store-relative #,(identity x) \
                     drop \
                     load-constant 2 \
                     store-relative #,(identity y) \
                     drop \
                     load-constant () \
                     load-relative #,(identity y) \
                     cons \
                     load-relative #,(identity x) \
                     cons \
                     load-absolute #,(identity +) \
                     tail-call) \
       tail-call)")

; ------------------------------------------------------------------------------

(define (f)
  ((lambda (x y)
     (set! x 1)
     (set! y 2)
     (+ x y))
   '()
   '()))

(check (object->string (car f))
  => "(load-constant (() ()) \
       load-closure (load-constant 1 \
                     store-relative #,(identity x) \
                     drop \
                     load-constant 2 \
                     store-relative #,(identity y) \
                     drop \
                     load-constant () \
                     load-relative #,(identity y) \
                     cons \
                     load-relative #,(identity x) \
                     cons \
                     load-absolute #,(identity +) \
                     tail-call) \
       tail-call)")

; ------------------------------------------------------------------------------

(define (f)
  (let ()
    (let ()
      (let () 42))))

(check (object->string (car f))
  => "(load-constant () \
       load-closure (load-constant () \
                     load-closure (load-constant () \
                                   load-closure (load-constant 42 \
                                                 return) \
                                   tail-call) \
                     tail-call) \
       tail-call)")

; ------------------------------------------------------------------------------

(define (f)
  (letrec ((a 1)
           (b 2))
    (+ a b)))

(check (object->string (car f))
  => "(dummy \
       load-constant (1 2) \
       load-closure (load-constant () \
                     load-relative #,(identity b) \
                     cons \
                     load-relative #,(identity a) \
                     cons \
                     load-absolute #,(identity +) \
                     tail-call) \
       tail-letrec)")

; ------------------------------------------------------------------------------

(define (f)
  (begin (+ 1 2)
         (+ 3 4)
         (+ 5 6)))

(check (object->string (car f))
  => "(load-constant (1 2) \
       load-absolute #,(identity +) \
       call \
       drop \
       load-constant (3 4) \
       load-absolute #,(identity +) \
       call \
       drop \
       load-constant (5 6) \
       load-absolute #,(identity +) \
       tail-call)")

; ------------------------------------------------------------------------------

(define (f)
  (begin (begin (+ 1 2))
         (begin (+ 3 4))
         (begin (+ 5 6))))

(check (object->string (car f))
  => "(load-constant (1 2) \
       load-absolute #,(identity +) \
       call \
       drop \
       load-constant (3 4) \
       load-absolute #,(identity +) \
       call \
       drop \
       load-constant (5 6) \
       load-absolute #,(identity +) \
       tail-call)")

; ------------------------------------------------------------------------------

(check-report)

(exit (check-passed? 9))
