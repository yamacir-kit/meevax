(import (scheme base)
        (scheme file)
        (scheme write)
        (scheme process-context)
        (srfi 78))

(define-library (test 1)
  (import (only (scheme base) define quote))
  (export f (rename g h))
  (begin (define (f) 'f)
         (define (g) 'g)))

(import (test 1))

(check (f) => 'f)

(check (h) => 'g)

(define-library (test 2))

(with-output-to-file "/tmp/hoge.ss"
  (lambda ()
    (write '(import (test 2)))
    (write '(define-syntax swap!
              (syntax-rules ()
                ((swap! a b)
                 (let ((tmp a))
                   (set! a b)
                   (set! b tmp))))))))

(with-output-to-file "/tmp/fuga.ss"
  (lambda ()
    (write '(define x 1))
    (write '(DEFINE Y 2))
    (write '(swap! x y))))

(include-ci "/tmp/hoge.ss"
            "/tmp/fuga.ss")

(check (cons x y) => '(2 . 1))

(with-output-to-file "/tmp/piyo.ss"
  (lambda ()
    (write '(import (scheme base)))
    (write '(export b))
    (write '(begin (define a 42)))))

(define-library (test 3)
  (include-library-declarations "/tmp/piyo.ss")
  (begin (define b (+ a 3.14))))

(import (test 3))

(check b => 45.14)

(check-report)

(exit (check-passed? 4))
