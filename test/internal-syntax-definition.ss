(import (only (meevax macro-transformer) sc-macro-transformer)
        (scheme base)
        (scheme process-context)
        (srfi 78))

(check (let ()
         (define-syntax m
           (syntax-rules ()
             ((m) 1)))
         (+ (m) 2))
  => 3)

(check (let ((x 1)
             (y 2))
         (define-syntax swap!
           (syntax-rules ()
             ((swap! a b)
              (let ((x a))
                (set! a b)
                (set! b x)))))
         (swap! x y)
         (list x y))
  => '(2 1))

(check (let ()
         (define (f x y)
           (+ x y))
         (define-syntax m
           (syntax-rules ()
             ((m a b)
              (f a b))))
         (m 1 2))
  => 3)

(check (let ()
         (define (f x y)
           (+ x y))
         (define-syntax m
           (syntax-rules ()
             ((m a b)
              (f a b))))
         (define (g x y)
           (m x y))
         (g 1 2))
  => 3)

(check (let ()
         (letrec* ((f (lambda (x y)
                        (+ x y))))
           (letrec-syntax ((m (syntax-rules ()
                                ((m a b)
                                 (f a b)))))
             (letrec* ((g (lambda (x y)
                            (m x y))))
               (g 1 2)))))
  => 3)

(check (let ()
         (define (f x y)
           (+ x y))
         (define (h x y)
           (g x y))
         (define-syntax m
           (syntax-rules ()
             ((m a b)
              (f a b))))
         (define (g x y)
           (m x y))
         (h 1 2))
  => 3)

(define-syntax with-internal-definition
  (sc-macro-transformer
    (lambda (form environment)
      '((lambda ()
          (begin (define x 'inner))
          x)))))

(check (let ((begin #f)
             (define #f)
             (x 'outer))
         (with-internal-definition))
  => 'inner)

(check-report)

(exit (check-passed? 7))
