; ==== SRFI-149 Basic syntax-rules template extension ==========================
;
; NOTE
;   Based on Chibi-Scheme's implementation at init-7.scm
;
; ORIGINAL LICENSE
;   Copyright (c) 2009-2018 Alex Shinn
;   All rights reserved.
;
;   Redistribution and use in source and binary forms, with or without
;   modification, are permitted provided that the following conditions
;   are met:
;   1. Redistributions of source code must retain the above copyright
;      notice, this list of conditions and the following disclaimer.
;   2. Redistributions in binary form must reproduce the above copyright
;      notice, this list of conditions and the following disclaimer in the
;      documentation and/or other materials provided with the distribution.
;   3. The name of the author may not be used to endorse or promote products
;      derived from this software without specific prior written permission.
;
;   THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;   IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;   IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;   NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;   THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;
; ==============================================================================

(define syntax-rules-transformer
  (lambda (form rename compare)
    (let ((ellipsis-specified? (identifier? (cadr form)))
          (count 0)
          (%- (rename '-))
          (%>= (rename '>=))
          (%and (rename 'and))
          (%append (rename 'append))
          (%apply (rename 'apply))
          (%begin (rename 'begin))
          (%car (rename 'car))
          (%cdr (rename 'cdr))
          (%compare (rename 'compare))
          (%cons (rename 'cons))
          ; (%cons-source (rename 'cons-source))
          (%eq? (rename 'eq?))
          (%equal? (rename 'equal?))
          (%er-macro-transformer (rename 'er-macro-transformer))
          (%error (rename 'error))
          (%form (rename 'form))
          (%i (rename 'i))
          (%if (rename 'if))
          (%lambda (rename 'lambda))
          (%len (rename 'len))
          ; (%length (rename 'length*))
          (%let (rename 'let))
          (%list->vector (rename 'list->vector))
          (%list? (rename 'list?))
          (%ls (rename 'ls))
          (%map (rename 'map))
          (%null? (rename 'null?))
          (%or (rename 'or))
          (%pair? (rename 'pair?))
          ; (%quote (rename 'syntax-quote))
          (%quote (rename 'quote))
          (%rename (rename 'rename))
          (%res (rename 'res))
          (%reverse (rename 'reverse))
          (%underscore (rename '%))
          (%vector->list (rename 'vector->list))
          (%vector? (rename 'vector?))
          )

      (define (any pred x)
        (if (pair? x)
            (if (null? (cdr x))
                (pred (car x))
                (or (pred (car x))
                    (any pred (cdr x))))
            #f))

      (define (length* x)
        (let ((r (length x)))
          (cond
            ((positive? r) r)
            ((= r -2) #f) ; is circular-list
            (else
              (let rec ((i 0)
                        (x x))
                (if (not (pair? x)) i
                    (rec (+ i 1)
                         (cdr x))))))))

      (define (%cons-source x y z)
        (cons x y))

      (define ellipsis
        (if ellipsis-specified? (cadr form) (rename '...)))

      (define lits
        (if ellipsis-specified? (car (cddr form)) (cadr form)))

      (define forms
        (if ellipsis-specified? (cdr (cddr form)) (cddr form)))

      (define (next-symbol s)
        (set! count (+ count 1))
        (rename (string->symbol (string-append s (number->string count)))))

      (define (expand-pattern pat tmpl)
        (let lp ((p (cdr pat))
                 (x (list %cdr %form))
                 (dim 0)
                 (vars '())
                 (k (lambda (vars)
                      (list %cons (expand-template tmpl vars) #f))))
          (let ((v (next-symbol "v.")))
            (list %let (list (list v x))
              (cond
                ((identifier? p)
                 (cond
                   ((ellipsis-mark? p)
                    (error "bad ellipsis" p))
                   ((memq p lits)
                    (list %and
                          (list %compare v (list %rename (list %quote p)))
                          (k vars)))
                   ((compare p %underscore)
                    (k vars))
                   (else
                     (list %let (list (list p v)) (k (cons (cons p dim) vars))))))
                ((ellipsis? p)
                 (cond
                   ((not (null? (cdr (cdr p))))
                    (cond
                      ((any (lambda (x)
                              (and (identifier? x)
                                   (ellipsis-mark? x)))
                            (cddr p))
                       (error "multiple ellipses" p))
                      (else
                        (let ((len (length* (cdr (cdr p))))
                              (%lp (next-symbol "lp.")))
                          `(,%let ((,%len (,%length ,v)))
                                  (,%and (,%>= ,%len ,len)
                                         (,%let ,%lp ((,%ls ,v)
                                                      (,%i (,%- ,%len ,len))
                                                      (,%res (,%quote ())))
                                                (,%if (,%>= 0 ,%i)
                                                      ,(lp `(,(cddr p)
                                                              (,(car p) ,(car (cdr p))))
                                                           `(,%cons ,%ls
                                                                    (,%cons (,%reverse ,%res)
                                                                            (,%quote ())))
                                                           dim
                                                           vars
                                                           k)
                                                      (,%lp (,%cdr ,%ls)
                                                            (,%- ,%i 1)
                                                            (,%cons-source (,%car ,%ls)
                                                                           ,%res
                                                                           ,%ls))))))))))
                   ((identifier? (car p))
                    (list %and (list %list? v)
                          (list %let (list (list (car p) v))
                                (k (cons (cons (car p) (+ 1 dim)) vars)))))
                   (else
                     (let* ((w (next-symbol "w."))
                            (%lp (next-symbol "lp."))
                            (new-vars (all-vars (car p) (+ dim 1)))
                            (ls-vars (map (lambda (x)
                                            (next-symbol
                                              (string-append
                                                (symbol->string
                                                  (identifier->symbol (car x)))
                                                "-ls")))
                                          new-vars))
                            (once
                              (lp (car p) (list %car w) (+ dim 1) '()
                                  (lambda (_)
                                    (cons
                                      %lp
                                      (cons
                                        (list %cdr w)
                                        (map (lambda (x l)
                                               (list %cons (car x) l))
                                             new-vars
                                             ls-vars)))))))
                       (list
                         %let
                         %lp (cons (list w v)
                                   (map (lambda (x) (list x (list %quote '()))) ls-vars))
                         (list %if (list %null? w)
                               (list %let (map (lambda (x l)
                                                 (list (car x) (list %reverse l)))
                                               new-vars
                                               ls-vars)
                                     (k (append new-vars vars)))
                               (list %and (list %pair? w) once)))))))
                ((pair? p)
                 (list %and (list %pair? v)
                       (lp (car p)
                           (list %car v)
                           dim
                           vars
                           (lambda (vars)
                             (lp (cdr p) (list %cdr v) dim vars k)))))
                ((vector? p)
                 (list %and
                       (list %vector? v)
                       (lp (vector->list p) (list %vector->list v) dim vars k)))
                ((null? p) (list %and (list %null? v) (k vars)))
                (else (list %and (list %equal? v p) (k vars))))))))

      (define ellipsis-mark?
        (if (if ellipsis-specified?
                (memq ellipsis lits)
                (any (lambda (x) (compare ellipsis x)) lits))
            (lambda (x) #f)
            (if ellipsis-specified?
                (lambda (x) (eq? ellipsis x))
                (lambda (x) (compare ellipsis x)))))

      (define (ellipsis-escape? x)
        (and (pair? x)
             (ellipsis-mark? (car x))))

      (define (ellipsis? x)
        (and (pair? x)
             (pair? (cdr x))
             (ellipsis-mark? (cadr x))))

      (define (ellipsis-depth x)
        (if (ellipsis? x)
            (+ 1 (ellipsis-depth (cdr x)))
            0))

      (define (ellipsis-tail x)
        (if (ellipsis? x)
            (ellipsis-tail (cdr x))
            (cdr x)))

      (define (all-vars x dim)
        (let lp ((x x) (dim dim) (vars '()))
          (cond ((identifier? x)
                 (if (or (memq x lits)
                         (compare x %underscore))
                     vars
                     (cons (cons x dim) vars)))
            ((ellipsis? x) (lp (car x) (+ dim 1) (lp (cddr x) dim vars)))
            ((pair? x)     (lp (car x)    dim    (lp (cdr x) dim vars)))
            ((vector? x) (lp (vector->list x) dim vars))
            (else vars))))

      (define (free-vars x vars dim)
        (let lp ((x x) (free '()))
          (cond
            ((identifier? x)
             (if (and (not (memq x free))
                      (cond ((assq x vars) => (lambda (cell) (>= (cdr cell) dim)))
                        (else #f)))
                 (cons x free)
                 free))
            ((pair? x) (lp (car x) (lp (cdr x) free)))
            ((vector? x) (lp (vector->list x) free))
            (else free))))

      (define (expand-template tmpl vars)
        (let lp ((t tmpl) (dim 0) (ell-esc #f))
          (cond
            ((identifier? t)
             (cond
               ((find (lambda (v) (eq? t (car v))) vars)
                => (lambda (cell)
                     (if (<= (cdr cell) dim)
                         t
                         (error "too few ...'s"))))
               (else
                 (list %rename (list %quote t)))))
            ((pair? t)
             (cond
               ((and (ellipsis-escape? t) (not ell-esc))
                (lp (if (and (pair? (cdr t)) (null? (cddr t))) (cadr t) (cdr t)) dim #t))
               ((and (ellipsis? t) (not ell-esc))
                (let* ((depth (ellipsis-depth t))
                       (ell-dim (+ dim depth))
                       (ell-vars (free-vars (car t) vars ell-dim)))
                  (cond
                    ((null? ell-vars)
                     (error "too many ...'s"))
                    ((and (null? (cdr (cdr t))) (identifier? (car t)))
                     ;; shortcut for (var ...)
                     (lp (car t) ell-dim ell-esc))
                    (else
                      (let* ((once (lp (car t) ell-dim ell-esc))
                             (nest (if (and (null? (cdr ell-vars))
                                            (identifier? once)
                                            (eq? once (car vars)))
                                       once ;; shortcut
                                       (cons %map
                                             (cons (list %lambda ell-vars once)
                                                   ell-vars))))
                             (many (do ((d depth (- d 1))
                                        (many nest
                                              (list %apply %append many)))
                                     ((= d 1) many))))
                        (if (null? (ellipsis-tail t))
                            many ;; shortcut
                            (list %append many (lp (ellipsis-tail t) dim ell-esc))))))))
               (else (list %cons-source (lp (car t) dim ell-esc) (lp (cdr t) dim ell-esc) (list %quote t)))))
            ((vector? t) (list %list->vector (lp (vector->list t) dim ell-esc)))
            ((null? t) (list %quote '()))
            (else t))))

      (list %er-macro-transformer
        (list %lambda (list %form %rename %compare)
          (list %car (cons %or
                           (append (map
                                     (lambda (clause)
                                       (expand-pattern (car clause) (cadr clause)))
                                     forms)
                                   (list
                                     (list %cons
                                           (list %error "no expansion for"
                                                 (list (rename 'strip-syntactic-closures) %form))
                                           #f)))))))
      ; `(,%er-macro-transformer
      ;    (,%lambda (,%form ,%rename ,%compare)
      ;      (,%car (,%or ,@(append
      ;                       (map (lambda (clause)
      ;                              (expand-pattern (car clause) (cadr clause)))
      ;                            forms)
      ;                       `((,%error "no expansion for"
      ;                                  (,%strip-syntactic-closures ,%form))
      ;                         #f))))))
      )
    )
  )

(define-syntax syntax-rules
  (er-macro-transformer
    (lambda (form rename compare)
      (syntax-rules-transformer form rename compare))))

; (define-syntax cond
;   (syntax-rules (else =>)
;     ((cond (else result1 result2 ...))
;      (begin result1 result2 ...))
;     ((cond (test => result))
;      (let ((temp test))
;        (if temp (result temp))))
;     ((cond (test => result) clause1 clause2 ...)
;      (let ((temp test))
;        (if temp
;            (result temp)
;            (cond clause1 clause2 ...))))
;     ((cond (test)) test)
;     ((cond (test) clause1 clause2 ...)
;      (let ((temp test))
;        (if temp
;            temp
;            (cond clause1 clause2 ...))))
;     ((cond (test result1 result2 ...))
;      (if test (begin result1 result2 ...)))
;     ((cond (test result1 result2 ...)
;        clause1 clause2 ...)
;      (if test
;          (begin result1 result2 ...)
;          (cond clause1 clause2 ...)))))
;
; (define-syntax case
;   (syntax-rules (else =>)
;     ((case (key ...)
;        clauses ...)
;      (let ((atom-key (key ...)))
;        (case atom-key clauses ...)))
;     ((case key
;        (else => result))
;      (result key))
;     ((case key
;        (else result1 result2 ...))
;      (begin result1 result2 ...))
;     ((case key
;        ((atoms ...) result1 result2 ...))
;      (if (memv key '(atoms ...))
;          (begin result1 result2 ...)))
;     ((case key
;        ((atoms ...) => result))
;      (if (memv key '(atoms ...))
;          (result key)))
;     ((case key
;        ((atoms ...) => result)
;        clause clauses ...)
;      (if (memv key '(atoms ...))
;          (result key)
;          (case key clause clauses ...)))
;     ((case key
;        ((atoms ...) result1 result2 ...)
;        clause clauses ...)
;      (if (memv key '(atoms ...))
;          (begin result1 result2 ...)
;          (case key clause clauses ...)))))
;
; (define-syntax and
;   (syntax-rules ()
;     ((and) #t)
;     ((and test) test)
;     ((and test1 test2 ...)
;      (if test1 (and test2 ...) #f))))
;
; (define-syntax or
;   (syntax-rules ()
;     ((or) #f)
;     ((or test) test)
;     ((or test1 test2 ...)
;      (let ((x test1))
;        (if x x (or test2 ...))))))
;
; (define-syntax when
;   (syntax-rules ()
;     ((when test result1 result2 ...)
;      (if test
;          (begin result1 result2 ...)))))
;
; (define-syntax unless
;   (syntax-rules ()
;     ((unless test result1 result2 ...)
;      (if (not test)
;          (begin result1 result2 ...)))))
;
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
;
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
; (define-syntax begin
;   (syntax-rules ()
;     ((begin exp ...)
;      ((lambda () exp ...)))))
;
; (define-syntax begin
;   (syntax-rules ()
;     ((begin exp)
;      exp)
;     ((begin exp1 exp2 ...)
;      (call-with-values
;        (lambda () exp1)
;        (lambda args
;          (begin exp2 ...))))))
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
