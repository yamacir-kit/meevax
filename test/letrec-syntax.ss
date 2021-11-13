(import (gauche base)
        (scheme base)
        (srfi 78))

(letrec-syntax ((macro (er-macro-transformer
                         (lambda (form rename compare)
                           (and (identifier? (rename 'macro))
                                )
                           ))))
  (check (macro) => #t))

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

(check-report)
