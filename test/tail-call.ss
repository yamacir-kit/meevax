(import (only (meevax core) call-with-current-continuation!)
        (scheme base)
        (scheme process-context)
        (scheme read)
        (scheme write)
        (srfi 78))

(define (disassemble closure)
  (let ((output-port (open-output-string "")))
    (write (car closure) output-port)
    (let ((input-port (open-input-string (get-output-string output-port))))
      (read input-port))))

(define (f)
  (car '(a b)))

(check (disassemble f)
  => '(load-constant (a b)
       load-relative (0 . 0)
       load-absolute car
       tail-call))

(define (f)
  ((lambda (x)
     (+ x 1))
   42))

(check (disassemble f)
  => '(load-constant 42
       load-relative (0 . 0)
       load-closure (load-constant 1
                     load-relative (0 . 1)
                     load-relative (0 . 0)
                     load-absolute +
                     tail-call)
       tail-call))

(define (f)
  (let ((x 42))
    (+ x 1)))

(check (disassemble f)
  => '(load-constant 42
       load-relative (0 . 0)
       load-closure (load-constant 1
                     load-relative (0 . 1)
                     load-relative (0 . 0)
                     load-absolute +
                     tail-call)
       tail-call))

(define (f)
  (define x 1)
  (define y 2)
  (+ x y))

(check (disassemble f)
  => '(load-constant ()
       load-constant ()
       load-relative (0 . 0)
       load-closure (load-constant 1
                     store-relative (0 . 1)
                     drop
                     load-constant 2
                     store-relative (0 . 2)
                     drop
                     load-relative (0 . 2)
                     load-relative (0 . 1)
                     load-relative (0 . 0)
                     load-absolute +
                     tail-call)
       tail-call))

(define (f)
  ((lambda (x y)
     (set! x 1)
     (set! y 2)
     (+ x y))
   '()
   '()))

(check (disassemble f)
  => '(load-constant ()
       load-constant ()
       load-relative (0 . 0)
       load-closure (load-constant 1
                     store-relative (0 . 1)
                     drop
                     load-constant 2
                     store-relative (0 . 2)
                     drop
                     load-relative (0 . 2)
                     load-relative (0 . 1)
                     load-relative (0 . 0)
                     load-absolute +
                     tail-call)
       tail-call))

(define (f)
  (let ()
    (let ()
      (let () 42))))

(check (disassemble f)
  => '(load-relative (0 . 0)
       load-closure (load-relative (0 . 0)
                     load-closure (load-relative (0 . 0)
                                   load-closure (load-constant 42
                                                 load-relative (0 . 0)
                                                 tail-call)
                                   tail-call)
                     tail-call)
       tail-call))

(define (f)
  (letrec ((a 1)
           (b 2))
    (+ a b)))

(check (disassemble f)
  => '(load-constant #;unspecified
       load-constant #;unspecified
       load-relative (0 . 0)
       load-closure (load-constant 2
                     load-constant 1
                     load-relative (0 . 0)
                     load-closure (load-relative (0 . 1)
                                   store-relative (1 . 1)
                                   drop
                                   load-relative (0 . 2)
                                   store-relative (1 . 2)
                                   drop
                                   load-relative (1 . 2)
                                   load-relative (1 . 1)
                                   load-relative (0 . 0)
                                   load-absolute +
                                   tail-call)
                     tail-call)
       tail-call))

(define (f)
  (begin (+ 1 2)
         (+ 3 4)
         (+ 5 6)))

(check (disassemble f)
  => '(load-constant 2
       load-constant 1
       load-closure (load-constant 4
                     load-constant 3
                     load-closure (load-constant 6
                                   load-constant 5
                                   load-relative (2 . 0)
                                   load-absolute +
                                   tail-call)
                     load-absolute +
                     tail-call)
       load-absolute +
       tail-call))

(define (f)
  (begin (begin (+ 1 2))
         (begin (+ 3 4))
         (begin (+ 5 6))))

(check (disassemble f)
  => '(load-constant 2
       load-constant 1
       load-closure (load-constant 4
                     load-constant 3
                     load-closure (load-constant 6
                                   load-constant 5
                                   load-relative (2 . 0)
                                   load-absolute +
                                   tail-call)
                     load-absolute +
                     tail-call)
       load-absolute +
       tail-call))

(define (f)
  (call-with-current-continuation!
    (lambda (return)
      (return))))

(check (disassemble f)
  => '(load-closure (load-relative (0 . 1)
                     load-relative (1 . 0)
                     tail-call)
       load-relative (0 . 0)
       load-closure (load-relative (0 . 0)
                     load-relative (0 . 1)
                     tail-call)
       tail-call))

(define (f)
  (call-with-current-continuation
    (lambda (return)
      (return))))

(check (disassemble f)
  => '(load-closure (load-relative (0 . 0)
                     load-relative (0 . 1)
                     tail-call)
       load-relative (0 . 0)
       load-absolute call-with-current-continuation
       tail-call))

(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))

(check (disassemble ack)
  => '(load-constant 0
       load-relative (0 . 1)
       load-closure (load-relative (0 . 0)
                     select (load-constant 1
                             load-relative (1 . 2)
                             load-relative (1 . 0)
                             load-absolute +
                             tail-call)
                            (load-constant 0
                             load-relative (1 . 2)
                             load-closure (load-relative (0 . 0)
                                           select (load-constant 1
                                                   load-relative (2 . 1)
                                                   load-closure (load-constant 1
                                                                 load-relative (0 . 0)
                                                                 load-relative (3 . 0)
                                                                 load-absolute ack
                                                                 tail-call)
                                                   load-absolute -
                                                   tail-call)
                                                  (load-constant 1
                                                   load-relative (2 . 1)
                                                   load-closure (load-constant 1
                                                                 load-relative (3 . 2)
                                                                 load-closure (load-relative (0 . 0)
                                                                               load-relative (4 . 1)
                                                                               load-closure (load-relative (0 . 0)
                                                                                             load-relative (2 . 0)
                                                                                             load-relative (5 . 0)
                                                                                             load-absolute ack
                                                                                             tail-call)
                                                                               load-absolute ack
                                                                               tail-call)
                                                                 load-absolute -
                                                                 tail-call)
                                                   load-absolute -
                                                   tail-call))
                             load-absolute =
                             tail-call))
       load-absolute =
       tail-call))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(check (disassemble fib)
  => '(load-constant 2
       load-relative (0 . 1)
       load-closure (load-relative (0 . 0)
                     select (load-relative (1 . 1)
                             load-relative (1 . 0)
                             tail-call)
                            (load-constant 1
                             load-relative (1 . 1)
                             load-closure (load-relative (0 . 0)
                                           load-closure (load-constant 2
                                                         load-relative (3 . 1)
                                                         load-closure (load-relative (0 . 0)
                                                                       load-closure (load-relative (0 . 0)
                                                                                     load-relative (2 . 0)
                                                                                     load-relative (5 . 0)
                                                                                     load-absolute +
                                                                                     tail-call)
                                                                       load-absolute fib
                                                                       tail-call)
                                                         load-absolute -
                                                         tail-call)
                                           load-absolute fib
                                           tail-call)
                             load-absolute -
                             tail-call))
       load-absolute <
       tail-call))

(check-report)

(exit (check-passed? 13))
