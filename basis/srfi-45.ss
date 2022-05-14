(define-library (srfi 45) ; Based on r7rs reference implementation.
  (import (scheme r4rs)
          (meevax syntax) ; for define-syntax
          (srfi 211 explicit-renaming))

  (export delay eager force lazy promise?)

  (begin (define <promise> (list 'promise))

         (define (promise done? value)
           (cons <promise> (cons done? value)))

         (define (promise? x)
           (and (pair? x)
                (eq? <promise> (car x))))

         (define promise-done? cadr)

         (define promise-value cddr)

         (define (promise-update! new old)
           (set-car! (cdr old) (promise-done? new))
           (set-cdr! (cdr old) (promise-value new))
           (set-car! new (cdr old)))

         (define (force promise)
           (if (promise-done? promise)
               (promise-value promise)
               ((lambda (promise*)
                  (if (not (promise-done? promise))
                      (promise-update! promise* promise))
                  (force promise))
                ((promise-value promise)))))

         (define-syntax lazy
           (er-macro-transformer
             (lambda (form rename compare)
               (list (rename 'promise) #f (list (rename 'lambda) '() (cadr form))))))

         (define-syntax delay
           (er-macro-transformer
             (lambda (form rename compare)
               (list (rename 'lazy) (list (rename 'promise) #t (cadr form))))))

         (define (eager x)
           (if (promise? x) x
               (delay x)))))
