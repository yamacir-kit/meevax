; (define-syntax receive
;   (syntax-rules ()
;     ((receive parameters expression . body)
;      (call-with-values
;        (lambda () expression)
;        (lambda parameters . body)))))

(define-syntax receive
  (experimental:er-macro-transformer
    (lambda (form rename compare)
      `(,(rename 'call-with-values)
         (,(rename 'lambda) () ,(caddr form))
         (,(rename 'lambda) ,(cadr form) ,@(cdddr form))))))
