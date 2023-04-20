(import (meevax macro-transformer)
        (scheme base)
        (scheme cxr)
        (scheme process-context)
        (srfi 78))

(define a 100)

(define b 200)

(let ()
  (define a 1)
  (define b 2)
  (check (+ a b) => 3))

(define hoge 'a)

(define fuga 'b)

(let ((define cons))
  (check (define hoge 'c) => '(a . c))
  (check (define fuga 'd) => '(b . d))
  (check hoge => 'a)
  (check fuga => 'b))

; ------------------------------------------------------------------------------

(define x 1)

(define y 2)

(let ()
  (define x 'a)
  (check x => 'a))

(let ()
  (begin (define x 'a))
  (check x => 'a))

(let ()
  (begin (begin (define x 'a)))
  (check x => 'a))

(let ()
  (begin (define x 'a)
         (define y 'b))
  (check x => 'a)
  (check y => 'b))

(let ()
  (begin (begin (define x 'a)
                (define y 'b)))
  (check x => 'a)
  (check y => 'b))

(let ()
  (begin (begin (define x 'a))
         (define y 'b))
  (check x => 'a)
  (check y => 'b))

(let ()
  (begin (define x 'a)
         (begin (define y 'b)))
  (check x => 'a)
  (check y => 'b))

(define-syntax %define
  (er-macro-transformer
    (lambda (form rename compare)
      `(,(rename 'define) ,@(cdr form)))))

(let ()
  (%define x 'a)
  (check x => 'a))

(let ()
  (begin (%define x 'a))
  (check x => 'a))

(let ()
  (begin (begin (%define x 'a)))
  (check x => 'a))

(let ()
  (begin (%define x 'a)
         (%define y 'b))
  (check x => 'a)
  (check y => 'b))

(let ()
  (begin (begin (%define x 'a)
                (%define y 'b)))
  (check x => 'a)
  (check y => 'b))

(let ()
  (begin (begin (%define x 'a))
         (%define y 'b))
  (check x => 'a)
  (check y => 'b))

(let ()
  (begin (%define x 'a)
         (begin (%define y 'b)))
  (check x => 'a)
  (check y => 'b))

(define-syntax %define-2
  (er-macro-transformer
    (lambda (form rename compare)
      `(,(rename 'begin) (,(rename 'define) ,(cadr form) ,@(cdddr form))
                         (,(rename 'define) ,(caddr form) ,@(cdddr form))))))

(let ()
  (%define-2 x y 'z)
  (check x => 'z)
  (check y => 'z))

(check-report)

(exit (check-passed? 29))
