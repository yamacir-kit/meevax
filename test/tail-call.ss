(import (only (meevax experimental) disassemble)
        (only (meevax function) closure?)
        (only (meevax syntax) call-with-current-continuation!)
        (scheme base)
        (scheme write)
        (scheme process-context)
        (srfi 78))

(define (object-code f)
  (if (closure? f)
      (parameterize ((current-output-port (open-output-string "")))
        (write (car f))
        (get-output-string (current-output-port)))
      ""))

; ------------------------------------------------------------------------------

(define (f)
  (car '(a b)))

(check (object-code f)
  => "(load-constant ((a b)) \
       load-absolute #,(identity car) \
       tail-call)")

; ------------------------------------------------------------------------------

(define (f)
  ((lambda (x)
     (+ x 1))
   42))

(check (object-code f)
  => "(load-constant (42) \
       load-closure (load-constant (1) \
                     load-relative #,(identity x) \
                     cons \
                     load-absolute #,(identity +) \
                     tail-call) \
       tail-call)")

; ------------------------------------------------------------------------------

(define (f)
  (let ((x 42))
    (+ x 1)))

(check (object-code f)
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

(check (object-code f)
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

(check (object-code f)
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

(check (object-code f)
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

(check (object-code f)
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

(check (object-code f)
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

(check (object-code f)
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
  (call-with-current-continuation!
    (lambda (return)
      (return))))

(check (object-code f)
  => "(load-continuation (return) \
       load-closure (load-constant () \
                     load-relative #,(identity return) \
                     tail-call) \
       tail-call)")

; ------------------------------------------------------------------------------

(define (f)
  (call-with-current-continuation
    (lambda (return)
      (return))))

(check (object-code f)
  => "(load-constant () \
       load-closure (load-constant () \
                     load-relative #,(identity return) \
                     tail-call) \
       cons \
       load-absolute #,(identity call-with-current-continuation) \
       tail-call)")

; ------------------------------------------------------------------------------

(check-report)

(exit (check-passed? 11))
