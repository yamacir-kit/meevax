; (import (scheme base))

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
                      '())))
               )
    (set! result (cons (f 0) result))

    (let ((f (lambda (a)
               (+ a 4))))
      (set! result (cons (f 0) result))
      )))

; (display result)
; (newline)

(check result => (4 3 2 1))

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

(check-report)

(exit (check-passed? check:correct))
