(import (scheme base)
        (scheme process-context)
        (srfi 78)
        (except (srfi 211 syntactic-closures) identifier?)
        (srfi 211 explicit-renaming))

(letrec-syntax ((my-and (er-macro-transformer
                          (lambda (form rename compare)
                            (cond ((null? (cdr form)) #t)
                                  ((null? (cddr form)) (cadr form))
                                  (else (list (rename 'if)
                                              (cadr form)
                                              (cons (rename 'my-and) (cddr form))
                                              #f)))))))
  (check (my-and #t #t #f #t) => #f))

(letrec-syntax ((my-or (er-macro-transformer
                         (lambda (form rename compare)
                           (cond ((null? (cdr form)) #f)
                                 ((null? (cddr form)) (cadr form))
                                 (else (list (rename 'let)
                                             (list (list (rename 'test) (cadr form)))
                                             (list (rename 'if)
                                                   (rename 'test)
                                                   (rename 'test)
                                                   (cons (rename 'my-or) (cddr form))))))))))
  (let ((x #f)
        (y 7)
        (temp 8)
        (let odd?)
        (if even?))
    (check (my-or x (let temp) (if y) y) => 7)))

; (check (let ((x 'outer))
;          (letrec-syntax ((m (er-macro-transformer
;                               (lambda (form rename compare)
;                                 (rename 'x)))))
;            (m))) => outer)
;
; (check (let ((x 'outer))
;          (letrec-syntax ((m (er-macro-transformer
;                               (lambda (form rename compare)
;                                 (rename 'x)))))
;            (let ((x 'inner))
;              (m)))) => outer)
;
; (check (let ((x 'outer))
;          (letrec-syntax ((m (er-macro-transformer
;                               (lambda (form rename compare)
;                                 (rename 'x)))))
;            (let ((x 'x1))
;              (let ((x 'x2))
;                (let ((x 'x3))
;                  (m)))))) => outer)
;
; (check (let ((x 'outer))
;          (letrec-syntax ((m (sc-macro-transformer
;                               (lambda (form use-env)
;                                 'x))))
;            (let ((x 'inner))
;              (m)))) => outer)
;
; (check (let ((x 'outer))
;          (letrec-syntax ((m (rsc-macro-transformer
;                               (lambda (form mac-env)
;                                 (make-syntactic-closure mac-env '() 'x)))))
;            (let ((x 'inner))
;              (m)))) => outer)

(check-report)

(exit (check-passed? 2))
