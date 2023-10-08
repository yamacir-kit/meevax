(define-library (srfi 45) ; Based on r7rs reference implementation.
  (import (only (meevax boolean) not)
          (only (meevax comparator) eq?)
          (only (meevax core) define define-syntax if lambda quote)
          (only (meevax list) list)
          (only (meevax macro-transformer) er-macro-transformer)
          (only (meevax pair) pair? cons car cdr cadr cddr set-car! set-cdr!))

  (export delay eager force lazy promise?)

  (begin (define <promise> (list 'promise))

         (define (promise done? value)
           (cons <promise> (cons done? value)))

         (define (promise? x)
           (if (pair? x)
               (eq? <promise> (car x))
               #f))

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
