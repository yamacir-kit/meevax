#|
   Copyright (C) Marc Feeley 2002. All Rights Reserved.

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to
   deal in the Software without restriction, including without limitation the
   rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
   sell copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
   IN THE SOFTWARE.
|#

(define-library (srfi 39)
  (import (only (meevax core) current install)
          (only (meevax continuation) dynamic-wind)
          (only (meevax core) define define-syntax if lambda letrec quote)
          (only (meevax list) null? list append assq)
          (only (meevax macro-transformer) er-macro-transformer-v2)
          (only (meevax map) map)
          (only (meevax pair) cons car cdr cadr cddr set-car! set-cdr!))

  (export make-parameter parameterize)

  (begin (define (current-dynamic-bindings)
           (current 1))

         (define (install-dynamic-bindings! bindings)
           (install 1 bindings))

         (define (make-parameter init . convert)
           ((lambda (convert)
              ((lambda (default)
                 (letrec ((parameter
                            (lambda value
                              ((lambda (cell)
                                 (if (null? value)
                                     (cdr cell)
                                     (if (null? (cdr value))
                                         (set-cdr! cell (convert (car value)))
                                         (convert (car value)))))
                               ((lambda (current-dynamic-binding)
                                  (if current-dynamic-binding
                                      current-dynamic-binding
                                      default))
                                (assq parameter (current-dynamic-bindings)))))))
                   (set-car! default parameter)
                   parameter))
               (cons #f (convert init))))
            (if (null? convert)
                (lambda (x) x)
                (car convert))))

         (define (dynamic-bind parameters values body)
           ((lambda (outer inner)
              (dynamic-wind (lambda () (install-dynamic-bindings! (append inner outer)))
                            body
                            (lambda () (install-dynamic-bindings! outer))))
            (current-dynamic-bindings)
            (map (lambda (parameter value)
                   (cons parameter (parameter value 'convert)))
                 parameters
                 values)))

         (define-syntax parameterize
           (er-macro-transformer-v2
             (lambda (form rename compare)
               (list (rename 'dynamic-bind)
                     (cons (rename 'list)
                           (map car (cadr form)))
                     (cons (rename 'list)
                           (map cadr (cadr form)))
                     (cons (rename 'lambda)
                           (cons '()
                                 (cddr form)))))))))
