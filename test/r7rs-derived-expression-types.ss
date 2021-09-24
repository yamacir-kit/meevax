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
