(define dynamic-environment '())

(define (make-parameter init . converter)
  (let* ((convert (if (null? converter)
                      (lambda (x) x)
                      (car converter)))
         (global-dynamic-environment (cons #f (convert init))))
    (define (dynamic-lookup parameter global-dynamic-environment)
      (or (assq parameter dynamic-environment) global-dynamic-environment))
    (define (parameter . value)
      (let ((binding (dynamic-lookup parameter global-dynamic-environment)))
        (cond ((null? value) (cdr binding))
              ((null? (cdr value)) (set-cdr! binding (convert (car value))))
              (else (convert (car value))))))
    (set-car! global-dynamic-environment parameter)
    parameter))

; (define-syntax parameterize
;   (er-macro-transformer
;     (lambda (form rename compare)
;       (let* ((bindings (cadr form))
;              (body (cddr form)))
;         `(dynamic-bind
;            (list ,@(map  car bindings))
;            (list ,@(map cadr bindings))
;            (lambda () ,@body))))))

(define-syntax (parameterize bindings . body)
  (define (dynamic-bind parameters values body)
    (let* ((saved dynamic-environment)
           (bindings (map (lambda (parameter value)
                            (cons parameter (parameter value #f)))
                          parameters
                          values)))
      (dynamic-wind
        (lambda () (set! dynamic-environment (append bindings saved)))
        body
        (lambda () (set! dynamic-environment saved)))))
  `(,dynamic-bind
     (,list ,@(map  car bindings))
     (,list ,@(map cadr bindings))
     (,lambda () ,@body)))
