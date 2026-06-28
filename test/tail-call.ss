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
  => '(load-constant ()
       load-constant (a b)
       cons
       load-absolute car
       tail-call))

(define (f)
  ((lambda (x)
     (+ x 1))
   42))

(check (disassemble f)
  => '(load-constant ()
       load-constant 42
       cons
       load-closure (load-constant ()
                     load-constant 1
                     cons
                     load-relative (0 . 0)
                     cons
                     load-absolute +
                     tail-call)
       tail-call))

(define (f)
  (let ((x 42))
    (+ x 1)))

(check (disassemble f)
  => '(load-constant ()
       load-constant 42
       cons
       load-closure (load-constant ()
                     load-constant 1
                     cons
                     load-relative (0 . 0)
                     cons
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
       cons
       load-constant ()
       cons
       load-closure (load-constant 1
                     store-relative (0 . 0)
                     drop
                     load-constant 2
                     store-relative (0 . 1)
                     drop
                     load-constant ()
                     load-relative (0 . 1)
                     cons
                     load-relative (0 . 0)
                     cons
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
       cons
       load-constant ()
       cons
       load-closure (load-constant 1
                     store-relative (0 . 0)
                     drop
                     load-constant 2
                     store-relative (0 . 1)
                     drop
                     load-constant ()
                     load-relative (0 . 1)
                     cons
                     load-relative (0 . 0)
                     cons
                     load-absolute +
                     tail-call)
       tail-call))

(define (f)
  (let ()
    (let ()
      (let () 42))))

(check (disassemble f)
  => '(load-constant ()
       load-closure (load-constant ()
                     load-closure (load-constant ()
                                   load-closure (load-constant 42
                                                 return)
                                   tail-call)
                     tail-call)
       tail-call))

(define (f)
  (letrec ((a 1)
           (b 2))
    (+ a b)))

(check (disassemble f)
  => '(dummy
       load-constant ()
       load-constant 2
       cons
       load-constant 1
       cons
       load-closure (load-constant ()
                     load-relative (0 . 1)
                     cons
                     load-relative (0 . 0)
                     cons
                     load-absolute +
                     tail-call)
       tail-letrec))

(define (f)
  (begin (+ 1 2)
         (+ 3 4)
         (+ 5 6)))

(check (disassemble f)
  => '(load-constant ()
       load-constant 2
       cons
       load-constant 1
       cons
       load-absolute +
       call
       drop
       load-constant ()
       load-constant 4
       cons
       load-constant 3
       cons
       load-absolute +
       call
       drop
       load-constant ()
       load-constant 6
       cons
       load-constant 5
       cons
       load-absolute +
       tail-call))

(define (f)
  (begin (begin (+ 1 2))
         (begin (+ 3 4))
         (begin (+ 5 6))))

(check (disassemble f)
  => '(load-constant ()
       load-constant 2
       cons
       load-constant 1
       cons
       load-absolute +
       call
       drop
       load-constant ()
       load-constant 4
       cons
       load-constant 3
       cons
       load-absolute +
       call
       drop
       load-constant ()
       load-constant 6
       cons
       load-constant 5
       cons
       load-absolute +
       tail-call))

(define (f)
  (call-with-current-continuation!
    (lambda (return)
      (return))))

(check (disassemble f)
  => '(load-continuation (return)
       load-closure (load-constant ()
                     load-relative (0 . 0)
                     tail-call)
       tail-call))

(define (f)
  (call-with-current-continuation
    (lambda (return)
      (return))))

(check (disassemble f)
  => '(load-constant ()
       load-closure (load-constant ()
                     load-relative (0 . 0)
                     tail-call)
       cons
       load-absolute call-with-current-continuation
       tail-call))

(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))

(check (disassemble ack)
  => '(load-constant ()
       load-constant 0
       cons
       load-relative (0 . 0)
       cons
       load-absolute =
       call
       tail-select (load-constant ()
                    load-constant 1
                    cons
                    load-relative (0 . 1)
                    cons
                    load-absolute +
                    tail-call)
                   (load-constant ()
                    load-constant 0
                    cons
                    load-relative (0 . 1)
                    cons
                    load-absolute =
                    call
                    tail-select (load-constant ()
                                 load-constant 1
                                 cons
                                 load-constant ()
                                 load-constant 1
                                 cons
                                 load-relative (0 . 0)
                                 cons
                                 load-absolute -
                                 call
                                 cons
                                 load-absolute ack
                                 tail-call)
                                (load-constant ()
                                 load-constant ()
                                 load-constant ()
                                 load-constant 1
                                 cons
                                 load-relative (0 . 1)
                                 cons
                                 load-absolute -
                                 call
                                 cons
                                 load-relative (0 . 0)
                                 cons
                                 load-absolute ack
                                 call
                                 cons
                                 load-constant ()
                                 load-constant 1
                                 cons
                                 load-relative (0 . 0)
                                 cons
                                 load-absolute -
                                 call
                                 cons
                                 load-absolute ack
                                 tail-call))))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(check (disassemble fib)
  => '(load-constant ()
       load-constant 2
       cons
       load-relative (0 . 0)
       cons
       load-absolute <
       call
       tail-select (load-relative (0 . 0)
                    return)
                   (load-constant ()
                    load-constant ()
                    load-constant ()
                    load-constant 2
                    cons
                    load-relative (0 . 0)
                    cons
                    load-absolute -
                    call
                    cons
                    load-absolute fib
                    call
                    cons
                    load-constant ()
                    load-constant ()
                    load-constant 1
                    cons
                    load-relative (0 . 0)
                    cons
                    load-absolute -
                    call
                    cons
                    load-absolute fib
                    call
                    cons
                    load-absolute +
                    tail-call)))

(check-report)

(exit (check-passed? 13))
