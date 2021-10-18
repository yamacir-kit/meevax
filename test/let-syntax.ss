; (import (scheme base))

(define result (list))

(define (f a) (+ a 1))

(set! result (cons (f 0) result))

(let ((f (lambda (a)
           (+ a 2))))
  (set! result (cons (f 0) result))

  (let-syntax ((f (er-macro-transformer
                    (lambda (form rename compare)
                      `(,(rename '+) ,(cadr form) 3)))))
    (set! result (cons (f 0) result))

    (let ((f (lambda (a)
               (+ a 4))))
      (set! result (cons (f 0) result))
      )))

; (display result)
; (newline)

(check result => (4 3 2 1))
