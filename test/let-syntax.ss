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

(check-report)

(exit (check-passed? 1))
