(define-library (srfi 211 syntactic-closures)
  (import (meevax macro)
          (meevax syntax))

  (begin (define (sc-macro-transformer f)
           (lambda (form use-env mac-env)
             (make-syntactic-closure mac-env '() (f form use-env))))

         (define (rsc-macro-transformer f)
           (lambda (form use-env mac-env)
             (make-syntactic-closure use-env '() (f form mac-env)))))

  (export sc-macro-transformer rsc-macro-transformer make-syntactic-closure identifier?))

(define-library (srfi 211 explicit-renaming)
  (import (rename (meevax comparator)
                  (identity=? eq?)
                  (normally=? eqv?))
          (meevax list)
          (meevax macro)
          (meevax pair)
          (meevax syntax))

  (begin (define (er-macro-transformer f)
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
             (f form rename compare))))

  (export er-macro-transformer identifier?))
