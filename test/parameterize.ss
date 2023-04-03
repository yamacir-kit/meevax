; http://blog.practical-scheme.net/shiro/20161215-parameterize

(import (scheme base)
        (scheme process-context)
        (scheme write)
        (srfi 78))

; ------------------------------------------------------------------------------

(define p1 (make-parameter 'default))

(check (procedure? p1) => #t)

(check (parameterize ((p1 'parameterized))
         (p1)) => 'parameterized)

(check (p1) => 'default)

; ------------------------------------------------------------------------------

(define continuation #f)

(define result #f)

(parameterize ((p1 'parameterized))
  (call/cc (lambda (cc)
             (set! continuation cc)))
  (set! result (p1)))

(check result => 'parameterized)

(check (p1) => 'default)

(continuation)

(check result => 'parameterized)

(check (p1) => 'default)

; ------------------------------------------------------------------------------

(parameterize ((p1 'parameterized))
  (p1 'before-call/cc)
  (call/cc (lambda (cc)
             (set! continuation cc)))
  (set! result (p1)))

(check result => 'before-call/cc)

(check (p1) => 'default)

(continuation)

(check result => 'before-call/cc)

(check (p1) => 'default)

; ------------------------------------------------------------------------------

(define p2 (make-parameter 1))

(parameterize ((p2 3))
  (set! p2 string->number))

; ------------------------------------------------------------------------------

(define p3 (make-parameter 1 -))

(check (p3) => -1)

(check (parameterize ((p3 -5))
         (p3)) => 5)

(parameterize ((p3 -5))
  (call/cc (lambda (cc)
             (set! continuation cc)))
  (set! result (p3)))

(check result => 5)

(check (p3) => -1)

(continuation)

(check result => 5)

(check (p3) => -1)

; ------------------------------------------------------------------------------

(define a (make-parameter 1 (lambda (x)
                              (unless (number? x)
                                (error "!!"))
                              x)))

(define b (make-parameter 2 (lambda (x)
                              (unless (number? x)
                                (error "!!"))
                              x)))

(check (call/cc (lambda (return)
                  (with-exception-handler
                    (lambda (x)
                      (return 'error-raised))
                    (lambda ()
                      (parameterize ((a 10)
                                     (b 'hoge))
                        (list a b)))))) => 'error-raised)

(check (a) => 1)

(check (b) => 2)

; ------------------------------------------------------------------------------

(check-report)

(exit (check-passed? 20))
