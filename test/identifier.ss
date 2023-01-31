(import (only (meevax macro) syntactic-closure?)
        (scheme base)
        (scheme process-context)
        (srfi 78)
        (srfi 211 explicit-renaming))

(define value 42)

; ------------------------------------------------------------------------------

(define-syntax er-macro-transformer:rename
  (er-macro-transformer
    (lambda (form rename compare)
      (rename (cadr form)))))

(check (identifier? 'value) => #t)

(check (identifier? 3) => #f)

(check value => 42)
(check (er-macro-transformer:rename value) => 42)

(let ((value 3.14))
  (check value => 3.14)
  (check (er-macro-transformer:rename value) => 42))

; ------------------------------------------------------------------------------

(define-syntax er-macro-transformer:rename-1
  (er-macro-transformer
    (lambda (form rename compare)
      (cons (rename (cadr form))
            (cddr form)))))

(check (+ 1 2 3 4) => 10)

(check (er-macro-transformer:rename-1 + 1 2 3 4) => 10)

; ------------------------------------------------------------------------------

(define-syntax %car
  (er-macro-transformer
    (lambda (form rename compare)
      (cons (rename 'car)
            (cdr form)))))

(check (car '(a b)) => a)

(check (%car '(a b)) => a)

; ------------------------------------------------------------------------------

(define-syntax er-macro-transformer:compare
  (er-macro-transformer
    (lambda (form rename compare)
      (let* ((x (cadr form))
             (y (rename x)))
        (check (identifier? x) => #t)
        (check (identifier? y) => #t)
        (check (symbol? x) => #t)
        (check (syntactic-closure? y) => #t)
        (compare x (rename x))
        )
      )
    )
  )

(check (er-macro-transformer:compare value) => #t)

(let ((value 3.14))
  (check (er-macro-transformer:compare value) => #f))

(check (er-macro-transformer:compare else) => #t)

; ------------------------------------------------------------------------------

; (check (identifier->symbol (identifier 'value)) => value)

(check-report)

(exit (check-passed? 25))
