(define-library (srfi 8)
  (import (scheme base))
  (export receive)
  (begin (define-syntax receive
           (syntax-rules ()
             ((receive parameters expression . body)
              (call-with-values
                (lambda () expression)
                (lambda parameters . body)))))))
