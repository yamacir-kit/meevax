#|
   Copyright (c) 2009-2018 Alex Shinn All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:
   1. Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
   2. Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
   3. The name of the author may not be used to endorse or promote products
      derived from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR IMPLIED
   WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
   EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
   OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
   WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
   OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
   ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

(import (meevax macro-transformer)
        (scheme base)
        (scheme process-context)
        (srfi 78))

; ---- chibi-scheme/tests/basic/test00-fact-3.scm ------------------------------

(define (fact-aux x res)
  (if (= x 0) res
      (fact-aux (- x 1) (* res x))))

(define (fact x)
  (fact-aux x 1))

(check (fact 3) => 6)

; ---- chibi-scheme/tests/basic/test01-apply.scm -------------------------------

(define foo
  (lambda (a b c d e f g h)
    (+ (+ (* a b)
          (* c d))
       (+ (* e f)
          (* g h)))))

(check (length (reverse (list 1 2 3 4 5 6 7 8 9 10 11))) => 11)

(check (reverse (list 1 2 3 4 5 6 7 8 9 10 11)) => '(11 10 9 8 7 6 5 4 3 2 1))

(check (append (list 1 2) (list 3 4)) => '(1 2 3 4))

(check (foo 1 2 3 4 5 6 7 8) => 100)

(check (apply foo (list 1 2 3 4 5 6 7 8)) => 100)

(check (apply foo 1 (list 2 3 4 5 6 7 8)) => 100)

(check (apply foo 1 2 3 4 (list 5 6 7 8)) => 100)

(check (apply foo 1 2 3 4 5 (list 6 7 8)) => 100)

; ---- chibi-scheme/tests/basic/test02-closure.scm -----------------------------

(define (make-counter n)
  (lambda ()
    (set! n (+ n 1))
    n))

(define f (make-counter 0))

(define g (make-counter 100))

(check (f) => 1)

(check (f) => 2)

(check (g) => 101)

(check (g) => 102)

(check (f) => 3)

(check (g) => 103)

; ---- chibi-scheme/tests/basic/test03-nested-closure.scm ----------------------

(check ((lambda (a b)
          ((lambda (c d e)
             (+ e (* c 1000) (* a 100) (* b 10) d))
           (- a 2) (+ b 2) 10000))
        3 5)
  => 11357)

; ---- chibi-scheme/tests/basic/test04-nested-let.scm --------------------------

(check (let ((a 3)
             (b 5))
         (let ((c (- a 2))
               (d (+ b 2))
               (e 10000))
           (+ e (* c 1000) (* a 100) (* b 10) d)))
  => 11357)

; ---- chibi-scheme/tests/basic/test05-internal-define.scm ---------------------

(check (let ((a 1000))
         (define b (+ a 3))
         (cons a b))
  => '(1000 . 1003))

; ---- chibi-scheme/tests/basic/test06-letrec.scm ------------------------------

(check (letrec ((add (lambda (a b)
                       (+ a b))))
         (add 3 4))
  => 7)

(check (letrec ((even? (lambda (n)
                         (if (zero? n) #t
                             (odd? (- n 1)))))
                (odd? (lambda (n)
                        (if (zero? n) #f
                            (even? (- n 1))))))
         (list (even? 1000)
               (even? 1001)
               (odd? 1000)))
  => '(#t #f #f))

; ---- chibi-scheme/tests/basic/test07-mutation.scm ----------------------------

(check (let ((a 3)
             (b 5))
         (let ((c (- a 2))
               (d (+ b 2))
               (e #f))
           (set! e 10000)
           (+ e (* c 1000) (* a 100) (* b 10) d)))
  => 11357)

; ---- chibi-scheme/tests/basic/test08-callcc.scm ------------------------------

(define fail
  (lambda () 999999))

(define in-range
  (lambda (a b)
    (call-with-current-continuation
      (lambda (cont)
        (enumerate a b cont)))))

(define enumerate
  (lambda (a b cont)
    (if (< b a)
        (fail)
        (let ((save fail))
          (begin (set! fail
                   (lambda ()
                     (begin (set! fail save)
                            (enumerate (+ a 1) b cont))))
                 (cont a))))))

(check (let ((x (in-range 2 9))
             (y (in-range 2 9))
             (z (in-range 2 9)))
         (if (= (* x x)
                (+ (* y y) (* z z)))
             (+ (* x 100) (+ (* y 10) z))
             (fail)))
  => 543)

; ---- chibi-scheme/tests/basic/test09-hygiene.scm -----------------------------

(check (or 1) => 1)

(check (or #f 2) => 2)

(check (or 3 #t) => 3)

(check (let ((tmp 4))
         (or #f tmp))
  => 4)

(check (letrec-syntax
         ((myor (er-macro-transformer
                  (lambda (expr rename compare)
                    (if (null? (cdr expr))
                        #f
                        (list (rename 'let) (list (list (rename 'tmp) (cadr expr)))
                              (list (rename 'if) (rename 'tmp)
                                    (rename 'tmp)
                                    (cons (rename 'myor) (cddr expr)))))))))
         (let ((tmp 5)) (myor #f tmp)))
  => 5)

(define-syntax myor
  (er-macro-transformer
    (lambda (expr rename compare)
      (if (null? (cdr expr)) #f
          (list (rename 'let) (list (list (rename 'tmp) (cadr expr)))
                (list (rename 'if) (rename 'tmp)
                      (rename 'tmp)
                      (cons (rename 'myor) (cddr expr))))))))

(check (let ((tmp 6))
         (myor #f tmp))
  => 6)

(check (let ((x 'outer))
         (let-syntax ((with-x
                        (syntax-rules ()
                          ((_ y expr)
                           (let-syntax ((y (syntax-rules () ((_) x))))
                             expr)))))
           (let ((x 'inner))
             (with-x z (z)))))
  => 'outer)

; ---- chibi-scheme/tests/basic/test10-unhygiene.scm ---------------------------

; (define-syntax aif
;   (sc-macro-transformer
;     (lambda (form environment)
;       (let ((condition
;               (make-syntactic-closure environment '() (cadr form)))
;             (consequent
;               (make-syntactic-closure environment '(it) (car (cddr form))))
;             (alternative
;               (make-syntactic-closure environment '() (cadr (cddr form)))))
;         `(let ((it ,condition))
;            (if it
;                ,consequent
;                ,alternative))))))
;
; (check (aif 1 it 3) => 1)
;
; (check
;   (let ((it 4))
;     (aif 1 it 3))
;   => 1)
;
; (check
;   (let ((it 4))
;     (aif (let ((it 5)) 1) it 3))
;   => 1)
;
; (check
;   (let ((it 4))
;     (aif (let ((it 5)) 1)
;          (let ((it 6)) it)
;          3))
;   => 6)

(check (letrec-syntax
         ((myor (er-macro-transformer
                  (lambda (expr rename compare)
                    (if (null? (cdr expr))
                        #f
                        (list (rename 'let) (list (list (rename 'it) (cadr expr)))
                              (list (rename 'if) (rename 'it)
                                    (rename 'it)
                                    (cons (rename 'myor) (cddr expr)))))))))
         (let ((it 7)) (myor #f it)))
  => 7)

; (define-syntax define-foo
;   (sc-macro-transformer
;     (lambda (form environment)
;       (make-syntactic-closure environment '(foo) `(define foo 8)))))
;
; (define-foo)
;
; (check foo => 8)

; ------------------------------------------------------------------------------

(check-report)

(exit (check-passed? 30))
