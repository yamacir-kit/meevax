(define-library (meevax macro-transformer)
  (import (meevax comparator)
          (meevax core)
          (meevax list)
          (meevax macro)
          (meevax pair))

  (export make-syntactic-closure identifier? sc-macro-transformer rsc-macro-transformer er-macro-transformer)

  (begin (define (sc-macro-transformer f)
           (lambda (form use-env mac-env)
             (make-syntactic-closure mac-env '() (f form use-env))))

         (define (rsc-macro-transformer f)
           (lambda (form use-env mac-env)
             (make-syntactic-closure use-env '() (f form mac-env))))

         (define (er-macro-transformer f)
           (lambda (form use-env mac-env)
             (define renames '())
             (define (rename x)
               (letrec ((assq (lambda (x alist)
                                (if (null? alist) #f
                                    (if (eq? x (caar alist))
                                        (car alist)
                                        (assq x (cdr alist))))))
                        (alist-cons (lambda (key x alist)
                                      (cons (cons key x) alist))))
                 (define key/value (assq x renames))
                 (if key/value
                     (cdr key/value)
                     (begin (set! renames (alist-cons x (make-syntactic-closure mac-env '() x) renames))
                            (cdar renames)))))
             (define (compare x y)
               (eqv? (if (syntactic-closure? x) x
                         (make-syntactic-closure use-env '() x))
                     (if (syntactic-closure? y) y
                         (make-syntactic-closure use-env '() y))))
             (f form rename compare)))))
