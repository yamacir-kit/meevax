; ---- 4.2.1. Conditionals -----------------------------------------------------

(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
           clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))

(define-syntax case ; errata version
  (syntax-rules (else =>)
    ((case (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key
       (else => result))
     (result key))
    ((case key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key
       ((atoms ...) => result))
     (if (memv key '(atoms ...))
         (result key)))
    ((case key
       ((atoms ...) => result)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (result key)
         (case key clause clauses ...)))
    ((case key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
    ((case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (case key clause clauses ...)))))

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))

(define-syntax when
  (syntax-rules ()
    ((when test result1 result2 ...)
     (if test
         (begin result1 result2 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless test result1 result2 ...)
     (if (not test)
         (begin result1 result2 ...)))))

; ---- 4.2.2. Binding constructs -----------------------------------------------

(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...) val ...))
    ((let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...) body1 body2 ...))) tag) val ...))))

(define-syntax let*
  (syntax-rules ()
    ((let* () body1 body2 ...)
     (let () body1 body2 ...))
    ((let* ((name1 val1) (name2 val2) ...) body1 body2 ...)
     (let ((name1 val1))
       (let* ((name2 val2) ...) body1 body2 ...)))))

(define-syntax letrec*
  (syntax-rules ()
    ((letrec* ((var1 init1) ...) body1 body2 ...)
     (let ((var1 <undefined>) ...)
       (set! var1 init1)
       ...
       (let () body1 body2 ...)))))

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

(define-syntax let-values
  (er-macro-transformer
    (lambda (form rename compare)
      (if (null? (cadr form))
          `(,(rename 'let) () ,@(cddr form))
          `(,(rename 'call-with-values)
             (,(rename 'lambda) () ,(cadar (cadr form)))
             (,(rename 'lambda) ,(caar (cadr form))
                                (,(rename 'let-values) ,(cdr (cadr form))
                                                       ,@(cddr form))))))))

; (define-syntax let*-values
;   (syntax-rules ()
;     ((let*-values () body0 body1 ...)
;      (let () body0 body1 ...))
;     ((let*-values (binding0 binding1 ...)
;                   body0 body1 ...)
;      (let-values (binding0)
;                  (let*-values (binding1 ...)
;                               body0 body1 ...)))))

(define-syntax let*-values
  (er-macro-transformer
    (lambda (form rename compare)
      (if (null? (cadr form))
          `(,(rename 'let) () ,@(cddr form))
          `(,(rename 'let-values) (,(caadr form))
                                  (,(rename 'let*-values) ,(cdadr form)
                                                          ,@(cddr form)))))))

; ---- 4.2.3. Sequencing -------------------------------------------------------

; ---- 4.2.4. Iteration --------------------------------------------------------

(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
       (test expr ...)
       command ...)
     (letrec
       ((loop
          (lambda (var ...)
            (if test
                (begin
                  (if #f #f)
                  expr ...)
                (begin
                  command
                  ...
                  (loop (do "step" var step ...)
                        ...))))))
       (loop init ...)))
    ((do "step" x)
     x)
    ((do "step" x y)
     y)))

; ---- 4.2.5. Delayed evaluation -----------------------------------------------

delay ; is defined in srfi-45.ss

(define delay-force lazy) ; lazy is defined in srfi-45.ss

force ; is defined in srfi-45.ss

promise? ; is defined in srfi-45.ss

make-promise ; is defined in srfi-45.ss

; ---- 4.2.6. Dynamic bindings -------------------------------------------------

make-parameter ; is defined in srfi-39.ss

parameterize ; is defined in srfi-39.ss

; ---- 4.2.7. Exception handling -----------------------------------------------

(define-syntax guard
  (syntax-rules ()
    ((guard (var clause ...) e1 e2 ...)
     ((call/cc
        (lambda (guard-k)
          (with-exception-handler
            (lambda (condition)
              ((call/cc
                 (lambda (handler-k)
                   (guard-k
                     (lambda ()
                       (let ((var condition))
                         (guard-aux
                           (handler-k
                             (lambda ()
                               (raise-continuable condition)))
                           clause ...))))))))
            (lambda ()
              (call-with-values
                (lambda () e1 e2 ...)
                (lambda args
                  (guard-k
                    (lambda ()
                      (apply values args)))))))))))))

(define-syntax guard-aux
  (syntax-rules (else =>)
    ((guard-aux reraise (else result1 result2 ...))
     (begin result1 result2 ...))
    ((guard-aux reraise (test => result))
     (let ((temp test))
       (if temp
           (result temp)
           reraise)))
    ((guard-aux reraise (test => result)
                clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test))
     (or test reraise))
    ((guard-aux reraise (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test result1 result2 ...))
     (if test
         (begin result1 result2 ...)
         reraise))
    ((guard-aux reraise
                (test result1 result2 ...)
                clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (guard-aux reraise clause1 clause2 ...)))))

; ---- 4.2.8. Quasiquotation ---------------------------------------------------

; ---- 4.2.9. Case-lambda ------------------------------------------------------

; ---- 6.1. Equivalence predicates ---------------------------------------------

; ---- 6.2. Numbers ------------------------------------------------------------

; TODO exact-integer-sqrt

; ---- 6.3. Booleans -----------------------------------------------------------

; ---- 6.4. Pairs and lists ----------------------------------------------------

(define list-tail drop) ; from SRFI-1

(define (list-set! x k object) (set-car! (list-tail x k) object))

; ---- 6.5 Symbols -------------------------------------------------------------

; ---- 6.6 Characters ----------------------------------------------------------

(define char-foldcase char-downcase)

; ---- 6.7 Strings -------------------------------------------------------------

(define (string-upcase   x) (string-map char-upcase   x))
(define (string-downcase x) (string-map char-downcase x))
(define (string-foldcase x) (string-map char-foldcase x))

; string-copy!

; ---- 6.8. Vectors ------------------------------------------------------------

; ---- 6.9. Bytevectors --------------------------------------------------------

(define (bytevector? x) #f)

; ---- 6.10. Control features --------------------------------------------------

(define (string-map f x . xs)
  (define (string-map-1 x)
    (list->string
      (map f (string->list x))))
  (define (string-map-n xs)
    (map list->string
         (map (lambda (c) (map f c))
              (map string->list xs))))
  (if (null? xs)
      (string-map-1 x)
      (string-map-n (cons x xs))))

; TODO vector-map

; TODO string-for-each
; TODO vector-for-each

; ---- 6.11. Exceptions --------------------------------------------------------

; ---- 6.12. Environments and evaluation ---------------------------------------

; TODO environment
; TODO scheme-report-environment
; TODO null-environment

; ---- 6.13. Input and output --------------------------------------------------

(define current-input-port
  (make-parameter (standard-input-port)
    (lambda (x)
      (cond ((not (input-port? x))
             (error "current-input-port: not input-port" x))
            ((not (input-port-open? x))
             (error "current-input-port: not input-port-open" x))
            (else x)))))

(define current-output-port
  (make-parameter (standard-output-port)
    (lambda (x)
      (cond ((not (output-port? x))
             (error "current-output-port: not output-port" x))
            ((not (output-port-open? x))
             (error "current-output-port: not output-port-open" x))
            (else x)))))

(define current-error-port
  (make-parameter (standard-error-port)
    (lambda (x)
      (cond ((not (output-port? x))
             (error "current-error-port: not output-port" x))
            ((not (output-port-open? x))
             (error "current-error-port: not output-port-open" x))
            (else x)))))

(define (with-input-from-file path thunk)
  (parameterize ((current-input-port (open-input-file path)))
    (thunk)))

(define (with-output-to-file path thunk)
  (parameterize ((current-output-port (open-output-file path)))
    (thunk)))

; TODO open-input-bytevector
; TODO open-output-bytevector
; TODO get-output-bytevector

; TODO write-u8
; TODO write-bytevector


; ---- 6.14. System interface --------------------------------------------------

; TODO file-exists?
; TODO delete-file
; TODO command-line

; ------------------------------------------------------------------------------
;
;  (exit)                                     process-context library procedure
;  (exit obj)                                 process-context library procedure
;
;  Runs all outstanding dynamic-wind after procedures, terminates the running
;  program, and communicates an exit value to the operating system. If no
;  argument is supplied, or if obj is #t, the exit procedure should communicate
;  to the operating system that the program exited normally. If obj is #f, the
;  exit procedure should communicate to the operating system that the program
;  exited abnormally. Otherwise, exit should translate obj into an appropriate
;  exit value for the operating system, if possible.
;
;  The exit procedure must not signal an exception or return to its
;  continuation.
;
;  Note: Because of the requirement to run handlers, this procedure is not just
;  the operating system’s exit procedure.
;
; ------------------------------------------------------------------------------

(define (exit . normally?)
  (for-each (lambda (before/after)
              ((cdr before/after)))
            %current-dynamic-extents)
  (apply emergency-exit normally?))

; TODO get-environment-variable
; TODO get-environment-variables

; TODO current-second
; TODO current-jiffy
; TODO jiffies-per-second
