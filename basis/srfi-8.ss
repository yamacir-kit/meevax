; (define-syntax receive
;   (syntax-rules ()
;     ((receive parameters expression . body)
;      (call-with-values
;        (lambda () expression)
;        (lambda parameters . body)))))

; (define-syntax receive
;   (%er-macro-transformer
;     (lambda (form rename compare)
;       `(call-with-values
;          (,(rename 'lambda) () ,(caddr form))
;          (,(rename 'lambda) ,(cadr form) ,@(cdddr form))))))

(define-syntax receive
  (hygienic-macro-transformer
    (lambda (receive parameters expression . body)
      (define (list . xs) xs)
      (list call-with-values
            (list lambda '() expression)
            (list lambda parameters . body)))))
