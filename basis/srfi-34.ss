;  Copyright (C) Richard Kelsey, Michael Sperber (2002). All Rights Reserved.
;
;  Permission is hereby granted, free of charge, to any person obtaining a copy
;  of this software and associated documentation files (the "Software"), to
;  deal in the Software without restriction, including without limitation the
;  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;  sell copies of the Software, and to permit persons to whom the Software is
;  furnished to do so, subject to the following conditions:
;
;  The above copyright notice and this permission notice shall be included in
;  all copies or substantial portions of the Software.
;
;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;  IN THE SOFTWARE.

(define-library (srfi 34)
  (import (only (meevax error) throw kernel-exception-handler-set!)
          (only (meevax syntax) current install)
          (scheme r5rs))

  (export with-exception-handler raise raise-continuable guard)

  (begin (define (current-exception-handlers)
           (current 2))

         (define (install-exception-handlers! handlers)
           (install 2 handlers))

         (define (with-exception-handlers new-handlers thunk)
           (let ((old-handlers (current-exception-handlers)))
             (dynamic-wind
               (lambda () (install-exception-handlers! new-handlers)) ; install
               thunk
               (lambda () (install-exception-handlers! old-handlers))))) ; uninstall

         (define (with-exception-handler handler thunk)
           (with-exception-handlers (cons handler (current-exception-handlers)) thunk))

         (define (raise x)
           (let ((inner (car (current-exception-handlers)))
                 (outer (cdr (current-exception-handlers))))
             (with-exception-handlers outer
               (lambda ()
                 (if (procedure? inner)
                     (inner x)
                     (throw x))
                 (throw x)))))

         (define (raise-continuable x)
           (let ((inner (car (current-exception-handlers)))
                 (outer (cdr (current-exception-handlers))))
             (with-exception-handlers outer
               (lambda ()
                 (if (procedure? inner)
                     (inner x)
                     (throw x))))))

         (kernel-exception-handler-set! raise)

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
                  (guard-aux reraise clause1 clause2 ...)))))))
