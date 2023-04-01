(import (scheme base)
        (scheme process-context)
        (srfi 211 explicit-renaming)
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
  (check (define hoge 'c) => (a . c))
  (check (define fuga 'd) => (b . d))
  (check hoge => a)
  (check fuga => b))

; ------------------------------------------------------------------------------

(define x 1)

(define y 2)

(let ()
  (define x 'a)
  (check x => a))

(let ()
  (begin (define x 'a))
  (check x => a))

(let ()
  (begin (begin (define x 'a)))
  (check x => a))

(let ()
  (begin (define x 'a)
         (define y 'b))
  (check x => a)
  (check y => b))

(let ()
  (begin (begin (define x 'a)
                (define y 'b)))
  (check x => a)
  (check y => b))

(let ()
  (begin (begin (define x 'a))
         (define y 'b))
  (check x => a)
  (check y => b))

(let ()
  (begin (define x 'a)
         (begin (define y 'b)))
  (check x => a)
  (check y => b))

; (define-syntax %define
;   (er-macro-transformer
;     (lambda (form rename compare)
;       `(,(rename 'define) ,@(cdr form))
;     )
;   )

(check-report)

(exit (check-passed? 16))
