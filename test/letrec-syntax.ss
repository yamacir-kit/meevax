(letrec-syntax ((macro (er-macro-transformer
                         (lambda (form rename compare)
                           (print "inside macro: " macro)
                           macro))))
  (print "outside macro: " (macro)))

; (check (letrec-syntax
;          ((my-or (syntax-rules ()
;                    ((my-or) #f)
;                    ((my-or e) e)
;                    ((my-or e1 e2 ...)
;                     (let ((temp e1))
;                       (if temp
;                           temp
;                           (my-or e2 ...)))))))
;          (let ((x #f)
;                (y 7)
;                (temp 8)
;                (let odd?)
;                (if even?))
;            (my-or x
;                   (let temp)
;                   (if y)
;                   y))) => 7)

(check-report)
