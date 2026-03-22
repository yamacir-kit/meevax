(define-library (meevax macro-transformer)
  (import (only (meevax comparator) eq? eqv?)
          (only (meevax core) begin define if lambda quote set!)
          (only (meevax list) assq null?)
          (only (meevax pair) cons car cdr caar cdar)
          (only (meevax syntactic-closure) identifier? syntactic-closure? make-syntactic-closure))

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

         (define (identifier=? environment1 identifier1
                               environment2 identifier2)
           (eqv? (if (syntactic-closure? identifier1) identifier1 (make-syntactic-closure environment1 '() identifier1))
                 (if (syntactic-closure? identifier2) identifier2 (make-syntactic-closure environment2 '() identifier2))))

         (define (er-macro-transformer f)
           (lambda (form use-env mac-env)
             (define renames '())
             (define (rename x)
               ((lambda (it)
                  (if it
                      (cdr it)
                      (begin (set! renames (cons (cons x (make-syntactic-closure mac-env '() x))
                                                 renames))
                             (cdar renames))))
                (assq x renames)))
             (define (compare x y)
               (identifier=? use-env x use-env y))
             (make-syntactic-closure use-env '() (f form rename compare))))))

(define-library (meevax continuation)
  (import (only (meevax context) emergency-exit)
          (only (meevax comparator) eq?)
          (only (meevax core) begin call-with-current-continuation! current define if install lambda)
          (only (meevax pair) caar car cdar cdr cons pair?)
          (only (meevax list) null?))

  (export call-with-current-continuation dynamic-wind exit)

  (begin (define (current-dynamic-extents)
           (current 0))

         (define (install-dynamic-extents! extents)
           (install 0 extents))

         (define (dynamic-wind before thunk after) ; https://www.cs.hmc.edu/~fleck/envision/scheme48/meeting/node7.html
           (before)
           (install-dynamic-extents! (cons (cons before after)
                                           (current-dynamic-extents)))
           ((lambda (result) ; TODO let-values
              (install-dynamic-extents! (cdr (current-dynamic-extents)))
              (after)
              result) ; TODO (apply values result)
            (thunk)))

         (define (call-with-current-continuation procedure)
           (define (windup! from to)
             (install-dynamic-extents! from)
             (if (eq? from to)
                 #t
                 (if (null? from)
                     (begin (windup! from (cdr to))
                            ((caar to)))
                     (if (null? to)
                         (begin ((cdar from))
                                (windup! (cdr from) to))
                         (begin ((cdar from))
                                (windup! (cdr from)
                                         (cdr to))
                                ((caar to))))))
             (install-dynamic-extents! to))
           ((lambda (dynamic-extents)
              (call-with-current-continuation!
                (lambda (continue)
                  (procedure (lambda (x)
                               (windup! (current-dynamic-extents) dynamic-extents)
                               (continue x))))))
            (current-dynamic-extents)))

         (define (exit . xs)
           (letrec ((for-each (lambda (f x)
                                (if (pair? x)
                                    (begin (f (car x))
                                           (for-each f (cdr x)))))))
             (for-each (lambda (before/after)
                         ((cdr before/after)))
                       (current-dynamic-extents))
             (emergency-exit . xs)))))

(define-library (meevax apply)
  (import (only (meevax core) define if lambda)
          (only (meevax list) append null? reverse)
          (only (meevax pair) car cdr cons))

  (export apply)

  (begin (define (apply f x . xs)
           (if (null? xs)
               (f . x)
               ((lambda (xs)
                  ((lambda (x)
                     (f . x))
                   (append (reverse (cdr xs))
                           (car xs))))
                (reverse (cons x xs)))))))

(define-library (meevax map)
  (import (only (meevax apply) apply)
          (only (meevax core) define if quote)
          (only (meevax list) memq null? reverse)
          (only (meevax pair) car cdr cons))

  (export map)

  (begin (define (map f . xs)
           (define (map f x a)
             (if (null? x)
                 (reverse a)
                 (map f
                      (cdr x)
                      (cons (f (car x)) a))))
           (define (map* f xs a)
             (if (memq '() xs)
                 (reverse a)
                 (map* f
                       (map cdr xs '())
                       (cons (apply f (map car xs '())) a))))
           (if (null? (cdr xs))
               (map f (car xs) '())
               (map* f xs '())))))
