; (define-syntax let
;   (syntax-rules ()
;     ((let ((name val) ...) body1 body2 ...)
;      ((lambda (name ...) body1 body2 ...)
;       val ...))
;     ((let tag ((name val) ...) body1 body2 ...)
;      ((letrec ((tag (lambda (name ...)
;                       body1 body2 ...)))
;         tag)
;       val ...))))

; (define-syntax let*
;   (syntax-rules ()
;     ((let* () body1 body2 ...)
;      (let () body1 body2 ...))
;     ((let* ((name1 val1) (name2 val2) ...)
;        body1 body2 ...)
;      (let ((name1 val1))
;        (let* ((name2 val2) ...)
;          body1 body2 ...)))))
;
; (define-syntax letrec
;   (syntax-rules ()
;     ((letrec ((var1 init1) ...) body ...)
;      (letrec "generate temp names"
;        (var1 ...)
;        ()
;        ((var1 init1) ...)
;        body ...))
;     ((letrec "generate temp names"
;        ()
;        (temp1 ...)
;        ((var1 init1) ...)
;        body ...)
;      (let ((var1 <undefined>) ...)
;        (let ((temp1 init1) ...)
;          (set! var1 temp1)
;          ...
;          body ...)))
;     ((letrec "generate temp names"
;        (x y ...)
;        (temp ...)
;        ((var1 init1) ...)
;        body ...)
;      (letrec "generate temp names"
;        (y ...)
;        (newtemp temp ...)
;        ((var1 init1) ...)
;        body ...))))
;
; (define-syntax letrec*
;   (syntax-rules ()
;     ((letrec* ((var1 init1) ...) body1 body2 ...)
;      (let ((var1 <undefined>) ...)
;        (set! var1 init1)
;        ...
;        (let () body1 body2 ...)))))
;
; (define-syntax let-values
;   (syntax-rules ()
;     ((let-values (binding ...) body0 body1 ...)
;      (let-values "bind"
;                  (binding ...) () (begin body0 body1 ...)))
;     ((let-values "bind" () tmps body)
;      (let tmps body))
;     ((let-values "bind" ((b0 e0)
;                          binding ...) tmps body)
;      (let-values "mktmp" b0 e0 ()
;                  (binding ...) tmps body))
;     ((let-values "mktmp" () e0 args
;                  bindings tmps body)
;      (call-with-values
;        (lambda () e0)
;        (lambda args
;          (let-values "bind"
;                      bindings tmps body))))
;     ((let-values "mktmp" (a . b) e0 (arg ...)
;                  bindings (tmp ...) body)
;      (let-values "mktmp" b e0 (arg ... x)
;                  bindings (tmp ... (a x)) body))
;     ((let-values "mktmp" a e0 (arg ...)
;                  bindings (tmp ...) body)
;      (call-with-values
;        (lambda () e0)
;        (lambda (arg ... . x)
;          (let-values "bind"
;                      bindings (tmp ... (a x)) body))))))
;
; (define-syntax let*-values
;   (syntax-rules ()
;     ((let*-values () body0 body1 ...)
;      (let () body0 body1 ...))
;     ((let*-values (binding0 binding1 ...)
;                   body0 body1 ...)
;      (let-values (binding0)
;                  (let*-values (binding1 ...)
;                               body0 body1 ...)))))
;
; (define-syntax define-values
;   (syntax-rules ()
;     ((define-values () expr)
;      (define dummy
;        (call-with-values (lambda () expr)
;                          (lambda args #f))))
;     ((define-values (var) expr)
;      (define var expr))
;     ((define-values (var0 var1 ... varn) expr)
;      (begin
;        (define var0
;          (call-with-values (lambda () expr)
;                            list))
;        (define var1
;          (let ((v (cadr var0)))
;            (set-cdr! var0 (cddr var0))
;            v)) ...
;        (define varn
;          (let ((v (cadr var0)))
;            (set! var0 (car var0))
;            v))))
;     ((define-values (var0 var1 ... . varn) expr)
;      (begin
;        (define var0
;          (call-with-values (lambda () expr)
;                            list))
;        (define var1
;          (let ((v (cadr var0)))
;            (set-cdr! var0 (cddr var0))
;            v)) ...
;        (define varn
;          (let ((v (cdr var0)))
;            (set! var0 (car var0))
;            v))))
;     ((define-values var expr)
;      (define var
;        (call-with-values (lambda () expr)
;                          list)))))
;
; ; (define-syntax begin
; ;   (syntax-rules ()
; ;     ((begin exp ...)
; ;      ((lambda () exp ...)))))
;
; ; (define-syntax begin
; ;   (syntax-rules ()
; ;     ((begin exp)
; ;      exp)
; ;     ((begin exp1 exp2 ...)
; ;      (call-with-values
; ;        (lambda () exp1)
; ;        (lambda args
; ;          (begin exp2 ...))))))
;
; (define-syntax do
;   (syntax-rules ()
;     ((do ((var init step ...) ...)
;        (test expr ...)
;        command ...)
;      (letrec
;        ((loop
;           (lambda (var ...)
;             (if test
;                 (begin
;                   (if #f #f)
;                   expr ...)
;                 (begin
;                   command
;                   ...
;                   (loop (do "step" var step ...)
;                         ...))))))
;        (loop init ...)))
;     ((do "step" x)
;      x)
;     ((do "step" x y)
;      y)))
;
; (define-syntax delay-force
;   (syntax-rules ()
;     ((delay-force expression)
;      (make-promise #f (lambda () expression)))))
;
; (define-syntax delay
;   (syntax-rules ()
;     ((delay expression)
;      (delay-force (make-promise #t expression)))))
;
; (define make-promise
;   (lambda (done? proc)
;     (list (cons done? proc))))
;
; (define (force promise)
;   (if (promise-done? promise)
;       (promise-value promise)
;       (let ((promise* ((promise-value promise))))
;         (unless (promise-done? promise)
;                 (promise-update! promise* promise))
;         (force promise))))
;
; (define promise-done?
;   (lambda (x) (car (car x))))
;
; (define promise-value
;   (lambda (x) (cdr (car x))))
;
; (define promise-update!
;   (lambda (new old)
;     (set-car! (car old) (promise-done? new))
;     (set-cdr! (car old) (promise-value new))
;     (set-car! new (car old))))
;
; (define (make-parameter init . o)
;   (let* ((converter
;            (if (pair? o) (car o) (lambda (x) x)))
;          (value (converter init)))
;     (lambda args
;       (cond
;         ((null? args)
;          value)
;         ((eq? (car args) <param-set!>)
;          (set! value (cadr args)))
;         ((eq? (car args) <param-convert>)
;          converter)
;         (else
;           (error "bad parameter syntax"))))))
;
; (define-syntax parameterize
;   (syntax-rules ()
;     ((parameterize ("step")
;                    ((param value p old new) ...)
;                    ()
;                    body)
;      (let ((p param) ...)
;        (let ((old (p)) ...
;                        (new ((p <param-convert>) value)) ...)
;          (dynamic-wind
;            (lambda () (p <param-set!> new) ...)
;            (lambda () . body)
;            (lambda () (p <param-set!> old) ...)))))
;     ((parameterize ("step")
;                    args
;                    ((param value) . rest)
;                    body)
;      (parameterize ("step")
;                    ((param value p old new) . args)
;                    rest
;                    body))
;     ((parameterize ((param value) ...) . body)
;      (parameterize ("step")
;                    ()
;                    ((param value) ...)
;                    body))))
;
; (define-syntax guard
;   (syntax-rules ()
;     ((guard (var clause ...) e1 e2 ...)
;      ((call/cc
;         (lambda (guard-k)
;           (with-exception-handler
;             (lambda (condition)
;               ((call/cc
;                  (lambda (handler-k)
;                    (guard-k
;                      (lambda ()
;                        (let ((var condition))
;                          (guard-aux
;                            (handler-k
;                              (lambda ()
;                                (raise-continuable condition)))
;                            clause ...))))))))
;             (lambda ()
;               (call-with-values
;                 (lambda () e1 e2 ...)
;                 (lambda args
;                   (guard-k
;                     (lambda ()
;                       (apply values args)))))))))))))
;
; (define-syntax guard-aux
;   (syntax-rules (else =>)
;     ((guard-aux reraise (else result1 result2 ...))
;      (begin result1 result2 ...))
;     ((guard-aux reraise (test => result))
;      (let ((temp test))
;        (if temp
;            (result temp)
;            reraise)))
;     ((guard-aux reraise (test => result)
;                 clause1 clause2 ...)
;      (let ((temp test))
;        (if temp
;            (result temp)
;            (guard-aux reraise clause1 clause2 ...))))
;     ((guard-aux reraise (test))
;      (or test reraise))
;     ((guard-aux reraise (test) clause1 clause2 ...)
;      (let ((temp test))
;        (if temp
;            temp
;            (guard-aux reraise clause1 clause2 ...))))
;     ((guard-aux reraise (test result1 result2 ...))
;      (if test
;          (begin result1 result2 ...)
;          reraise))
;     ((guard-aux reraise
;                 (test result1 result2 ...)
;                 clause1 clause2 ...)
;      (if test
;          (begin result1 result2 ...)
;          (guard-aux reraise clause1 clause2 ...)))))
;
; (define-syntax case-lambda
;   (syntax-rules ()
;     ((case-lambda (params body0 ...) ...)
;      (lambda args
;        (let ((len (length args)))
;          (let-syntax
;            ((cl (syntax-rules ::: ()
;                   ((cl)
;                    (error "no matching clause"))
;                   ((cl ((p :::) . body) . rest)
;                    (if (= len (length ’(p :::)))
;                        (apply (lambda (p :::)
;                                 . body)
;                               args)
;                        (cl . rest)))
;                   ((cl ((p ::: . tail) . body)
;                        . rest)
;                    (if (>= len (length ’(p :::)))
;                        (apply
;                          (lambda (p ::: . tail)
;                            . body)
;                          args)
;                        (cl . rest))))))
;            (cl (params body-2 ...) ...)))))))
;
; (define-syntax cond-expand
;   ;; Extend this to mention all feature ids and libraries
;   (syntax-rules (and or not else r7rs library scheme base)
;     ((cond-expand)
;      (syntax-error "Unfulfilled cond-expand"))
;     ((cond-expand (else body ...))
;      (begin body ...))
;     ((cond-expand ((and) body ...) more-clauses ...)
;      (begin body ...))
;     ((cond-expand ((and req1 req2 ...) body ...)
;                   more-clauses ...)
;      (cond-expand
;        (req1
;          (cond-expand
;            ((and req2 ...) body ...)
;            more-clauses ...))
;        more-clauses ...))
;     ((cond-expand ((or) body ...) more-clauses ...)
;      (cond-expand more-clauses ...))
;     ((cond-expand ((or req1 req2 ...) body ...)
;                   more-clauses ...)
;      (cond-expand
;        (req1
;          (begin body ...))
;        (else
;          (cond-expand
;            ((or req2 ...) body ...)
;            more-clauses ...))))
;     ((cond-expand ((not req) body ...)
;                   more-clauses ...)
;      (cond-expand
;        (req
;          (cond-expand more-clauses ...))
;        (else body ...)))
;     ((cond-expand (r7rs body ...)
;                   more-clauses ...)
;      (begin body ...))
;     ;; Add clauses here for each
;     ;; supported feature identifier.
;     ;; Samples:
;     ;; ((cond-expand (exact-closed body ...) more-clauses ...)
;     ;;  (begin body ...))
;     ;; ((cond-expand (ieee-float body ...) more-clauses ...)
;     ;;  (begin body ...))
;     ((cond-expand ((library (scheme base))
;                    body ...)
;                   more-clauses ...)
;      (begin body ...))
;     ;; Add clauses here for each library
;     ((cond-expand (feature-id body ...)
;                   more-clauses ...)
;      (cond-expand more-clauses ...))
;     ((cond-expand ((library (name ...))
;                    body ...)
;                   more-clauses ...)
;      (cond-expand more-clauses ...))))
;
