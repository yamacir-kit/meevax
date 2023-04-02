; Copyright (C) Marc Feeley 2002. All Rights Reserved.
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

(define-library (srfi 39)
  (import (only (meevax core) current install)
          (scheme r5rs)
          (srfi 211 explicit-renaming))

  (export make-parameter parameterize)

  (begin (define (current-dynamic-bindings)
           (current 1))

         (define (install-dynamic-bindings! bindings)
           (install 1 bindings))

         (define (make-parameter init . converter)
           (let* ((convert (if (null? converter)
                               (lambda (x) x)
                               (car converter)))
                  (default (cons #f (convert init))))
             (letrec ((parameter
                        (lambda value
                          (let ((cell (or (assq parameter (current-dynamic-bindings)) default)))
                            (cond ((null? value)
                                   (cdr cell))
                                  ((null? (cdr value))
                                   (set-cdr! cell (convert (car value))))
                                  (else ; Apply converter to value
                                    (convert (car value))))))))
               (set-car! default parameter)
               parameter)))

         (define (dynamic-bind parameters values body)
           (let* ((outer (current-dynamic-bindings))
                  (inner (map (lambda (parameter value)
                                (cons parameter (parameter value 'apply-converter-to-value)))
                              parameters
                              values)))
             (dynamic-wind (lambda () (install-dynamic-bindings! (append inner outer)))
                           body
                           (lambda () (install-dynamic-bindings! outer)))))

         (define-syntax parameterize
           (er-macro-transformer
             (lambda (form rename compare)
               `(,(rename 'dynamic-bind) (,(rename 'list) ,@(map car (cadr form)))
                                         (,(rename 'list) ,@(map cadr (cadr form)))
                                         (,(rename 'lambda) () ,@(cddr form))))))))
