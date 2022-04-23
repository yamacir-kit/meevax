; (import (scheme base))

(define result (list))

(define (f a) (+ a 1))

(set! result (cons (f 0) result))

(let ((f (lambda (a)
           (+ a 2))))
  (set! result (cons (f 0) result))
  (experimental:let-syntax ((f (experimental:er-macro-transformer
                                 (lambda (form rename compare)
                                   `(,(rename '+) ,(cadr form) 3))))
                            (g (experimental:er-macro-transformer
                                 (lambda (form rename compare)
                                   '()))))
    (set! result (cons (f 0) result))
    (let ((f (lambda (a)
               (+ a 4))))
      (set! result (cons (f 0) result)))))

(check result => (4 3 2 1))

; ------------------------------------------------------------------------------

(define y 100)

(define (double-y)
  (let ((+y (lambda (x) (+ x y))))
    (experimental:let-syntax ((macro (experimental:er-macro-transformer
                                       (lambda (form rename compare)
                                         `(,(rename '+) ,(cadr form) ,(+y 0))))))
      (macro y))))

(check (double-y) => 200)

(set! y 101)

(check (double-y) => 201)

; ------------------------------------------------------------------------------

; (check (let ((x 'outer))
;          (let-syntax ((m (er-macro-transformer
;                            (lambda (form rename compare)
;                              (rename 'x)))))
;            (let ((x 'inner))
;              (m)))) => outer)
;
; (define result
;   (let ((x 'outer))
;     (let-syntax ((m (er-macro-transformer
;                       (lambda (form rename compare)
;                         (rename 'x)))))
;       (let ((x 'inner))
;         (m)))))
;
; (check result => outer)

(check-report)

(exit (check-passed? check:correct))
