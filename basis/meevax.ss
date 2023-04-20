(define-library (meevax macro-transformer)
  (import (only (meevax comparator) eq? eqv?)
          (only (meevax core) begin define if lambda quote set!)
          (only (meevax list) null?)
          (only (meevax macro) identifier? syntactic-closure? make-syntactic-closure)
          (only (meevax pair) cons car cdr caar cdar))

  (export make-syntactic-closure
          identifier?
          identifier=?
          sc-macro-transformer
          rsc-macro-transformer
          er-macro-transformer)

  (begin (define (sc-macro-transformer f)
           (lambda (form use-env mac-env)
             (make-syntactic-closure mac-env '() (f form use-env))))

         (define (rsc-macro-transformer f)
           (lambda (form use-env mac-env)
             (make-syntactic-closure use-env '() (f form mac-env))))

         (define (assq x alist)
           (if (null? alist)
               #f
               (if (eq? x (caar alist))
                   (car alist)
                   (assq x (cdr alist)))))

         (define (identifier=? environment1 identifier1
                               environment2 identifier2)
           (eqv? (if (syntactic-closure? identifier1) identifier1 (make-syntactic-closure environment1 '() identifier1))
                 (if (syntactic-closure? identifier2) identifier2 (make-syntactic-closure environment2 '() identifier2))))

         (define (er-macro-transformer f)
           (lambda (form use-env mac-env)
             (define cache '())
             (f form
                (lambda (x)
                  ((lambda (pare)
                     (if pare
                         (cdr pare)
                         (begin (set! cache (cons (cons x (make-syntactic-closure mac-env '() x))
                                                  cache))
                                (cdar cache))))
                   (assq x cache)))
                (lambda (x y)
                  (identifier=? use-env x use-env y)))))))
