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
  => '(drop-values
       load-constant (a b)
       load-absolute car
       tail-call))

(define (f)
  ((lambda (x)
     (+ x 1))
   42))

(check (disassemble f)
  => '(drop-values
       load-constant 42
       load-closure (drop-values
                     load-constant 1
                     load-relative (0 . 0)
                     load-absolute +
                     tail-call)
       tail-call))

(define (f)
  (let ((x 42))
    (+ x 1)))

(check (disassemble f)
  => '(drop-values
       load-constant 42
       load-closure (drop-values
                     load-constant 1
                     load-relative (0 . 0)
                     load-absolute +
                     tail-call)
       tail-call))

(define (f)
  (define x 1)
  (define y 2)
  (+ x y))

(check (disassemble f)
  => '(drop-values
       load-constant ()
       load-constant ()
       load-closure (load-constant 1
                     store-relative (0 . 0)
                     drop
                     load-constant 2
                     store-relative (0 . 1)
                     drop
                     drop-values
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
  => '(drop-values
       load-constant ()
       load-constant ()
       load-closure (load-constant 1
                     store-relative (0 . 0)
                     drop
                     load-constant 2
                     store-relative (0 . 1)
                     drop
                     drop-values
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
  => '(drop-values
       load-closure (drop-values
                     load-closure (drop-values
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
  => '(drop-values
       dummy
       load-constant 2
       load-constant 1
       load-closure (drop-values
                     load-relative (0 . 1)
                     load-relative (0 . 0)
                     load-absolute +
                     tail-call)
       tail-letrec))

(define (f)
  (begin (+ 1 2)
         (+ 3 4)
         (+ 5 6)))

(check (disassemble f)
  => '(save-values
       load-constant 2
       load-constant 1
       load-absolute +
       call
       cons-values
       drop
       save-values
       load-constant 4
       load-constant 3
       load-absolute +
       call
       cons-values
       drop
       drop-values
       load-constant 6
       load-constant 5
       load-absolute +
       tail-call))

(define (f)
  (begin (begin (+ 1 2))
         (begin (+ 3 4))
         (begin (+ 5 6))))

(check (disassemble f)
  => '(save-values
       load-constant 2
       load-constant 1
       load-absolute +
       call
       cons-values
       drop
       save-values
       load-constant 4
       load-constant 3
       load-absolute +
       call
       cons-values
       drop
       drop-values
       load-constant 6
       load-constant 5
       load-absolute +
       tail-call))

(define (f)
  (call-with-current-continuation!
    (lambda (return)
      (return))))

(check (disassemble f)
  => '(drop-values
       load-continuation (return)
       load-closure (drop-values
                     load-relative (0 . 0)
                     tail-call)
       tail-call))

(define (f)
  (call-with-current-continuation
    (lambda (return)
      (return))))

(check (disassemble f)
  => '(drop-values
       load-closure (drop-values
                     load-relative (0 . 0)
                     tail-call)
       load-absolute call-with-current-continuation
       tail-call))

(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))

(check (disassemble ack)
  => '(save-values
       load-constant 0
       load-relative (0 . 0)
       load-absolute =
       call
       cons-values
       select (drop-values
               load-constant 1
               load-relative (0 . 1)
               load-absolute +
               tail-call)
              (save-values
               load-constant 0
               load-relative (0 . 1)
               load-absolute =
               call
               cons-values
               select (drop-values
                       load-constant 1
                       save-values
                       load-constant 1
                       load-relative (0 . 0)
                       load-absolute -
                       call
                       cons-values
                       load-absolute ack
                       tail-call)
                      (drop-values
                       save-values
                       save-values
                       load-constant 1
                       load-relative (0 . 1)
                       load-absolute -
                       call
                       cons-values
                       load-relative (0 . 0)
                       load-absolute ack
                       call
                       cons-values
                       save-values
                       load-constant 1
                       load-relative (0 . 0)
                       load-absolute -
                       call
                       cons-values
                       load-absolute ack
                       tail-call))))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(check (disassemble fib)
  => '(save-values
       load-constant 2
       load-relative (0 . 0)
       load-absolute <
       call
       cons-values
       select (load-relative (0 . 0)
               return)
              (drop-values
               save-values
               save-values
               load-constant 2
               load-relative (0 . 0)
               load-absolute -
               call
               cons-values
               load-absolute fib
               call
               cons-values
               save-values
               save-values
               load-constant 1
               load-relative (0 . 0)
               load-absolute -
               call
               cons-values
               load-absolute fib
               call
               cons-values
               load-absolute +
               tail-call)))

(check-report)

(exit (check-passed? 13))
