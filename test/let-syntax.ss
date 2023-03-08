(import (scheme base)
        (scheme process-context)
        (srfi 78)
        (srfi 211 explicit-renaming))

(define result (list))

(define (f a) (+ a 1))

(set! result (cons (f 0) result))

(let ((f (lambda (a)
           (+ a 2))))
  (set! result (cons (f 0) result))
  (let-syntax ((f (er-macro-transformer
                    (lambda (form rename compare)
                      `(,(rename '+) ,(cadr form) 3))))
               (g (er-macro-transformer
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
    (let-syntax ((macro (er-macro-transformer
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
; (check (let ((x 'outer))
;          (let-syntax ((m (er-macro-transformer
;                            (lambda (form rename compare)
;                              (rename 'x)))))
;            (let ((x 'x1))
;              (let ((x 'x2))
;                (let ((x 'x3))
;                  (m)))))) => outer)
;
; (let ((x 'outer))
;   (let-syntax ((m (er-macro-transformer
;                     (lambda (form rename compare)
;                       (rename 'x)))))
;     (let ((x 'x1))
;       (let ((x 'x2))
;         (let ((x 'x3))
;           (check (m) => outer))))))
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

(exit (check-passed? 3))
