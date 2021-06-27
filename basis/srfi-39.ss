; (define dynamic-environment '())
;
; (define (make-parameter init . converter)
;   (let* ((convert (if (null? converter)
;                       (lambda (x) x)
;                       (car converter)))
;          (global-dynamic-environment (cons #f (convert init))))
;     (define (dynamic-lookup parameter global-dynamic-environment)
;       (or (assq parameter dynamic-environment) global-dynamic-environment))
;     (define (parameter . value)
;       (let ((binding (dynamic-lookup parameter global-dynamic-environment)))
;         (cond ((null? value) (cdr binding))
;               ((null? (cdr value)) (set-cdr! binding (convert (car value))))
;               (else (convert (car value))))))
;     (set-car! global-dynamic-environment parameter)
;     parameter))

(define make-parameter
  (lambda (init . conv)
    (let ((converter
            (if (null? conv) (lambda (x) x) (car conv))))
      (let ((global-cell
              (cons #f (converter init))))
        (define parameter
          (lambda new-val
            (let ((cell (dynamic-lookup parameter global-cell)))
              (cond ((null? new-val)
                     (cdr cell))
                    ((null? (cdr new-val))
                     (set-cdr! cell (converter (car new-val))))
                    (else ; this case is needed for parameterize
                      (converter (car new-val)))))))
        (set-car! global-cell parameter)
        parameter))))

(define dynamic-bind
  (lambda (parameters values body)
    (let* ((old-local
             (dynamic-env-local-get))
           (new-cells
             (map (lambda (parameter value)
                    (cons parameter (parameter value #f)))
                  parameters
                  values))
           (new-local
             (append new-cells old-local)))
      (dynamic-wind
        (lambda () (dynamic-env-local-set! new-local))
        body
        (lambda () (dynamic-env-local-set! old-local))))))

(define dynamic-lookup
  (lambda (parameter global-cell)
    (or (assq parameter (dynamic-env-local-get))
        global-cell)))

(define dynamic-env-local '())

(define dynamic-env-local-get
  (lambda () dynamic-env-local))

(define dynamic-env-local-set!
  (lambda (new-env)
    (set! dynamic-env-local new-env)))

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
  ; (define (dynamic-bind parameters values body)
  ;   (let* ((saved dynamic-environment)
  ;          (bindings (map (lambda (parameter value)
  ;                           (cons parameter (parameter value #f)))
  ;                         parameters
  ;                         values)))
  ;     (dynamic-wind
  ;       (lambda () (set! dynamic-environment (append bindings saved)))
  ;       body
  ;       (lambda () (set! dynamic-environment saved)))))
  `(,dynamic-bind
     (,list ,@(map  car bindings))
     (,list ,@(map cadr bindings))
     (,lambda () ,@body)))
