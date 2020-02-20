(define passed 0)

(define expect
  (fork
    (lambda (this expects expression)
     `(,let ((,result ,expression)
             )
        (,if (,equal? ,result ',expects)
             (,begin (,set! passed (,+ passed 1))
                     ,result)
             (,begin (,display "; test\t\t; expected ")
                     (,display ',expects)
                     (,display " as result of ")
                     (,display ',expression)
                     (,display ", but got ")
                     (,display ,result)
                     (,newline)
                     (,emergency-exit)))))))

