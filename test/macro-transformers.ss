(import (meevax macro-transformer)
        (scheme base)
        (scheme cxr)
        (scheme process-context)
        (scheme write)
        (srfi 78))

(define (print . xs)
  (for-each display xs)
  (newline))

; ---- DEFINE-SYNTAX -----------------------------------------------------------

(define-syntax swap!
  (sc-macro-transformer
    (lambda (form environment)
      (let ((a (make-syntactic-closure environment '() (cadr form)))
            (b (make-syntactic-closure environment '() (caddr form))))
        `(let ((x ,a))
           (set! ,a ,b)
           (set! ,b x))))))

(let ((x 1)
      (y 2))
  (swap! x y)
  (check (cons x y) => '(2 . 1))
  (swap! x y)
  (check (cons x y) => '(1 . 2))
  (let ((a 'a)
        (b 'b)
        (let 'let)
        (set! 'set!))
    (swap! x y)
    (check (cons x y) => '(2 . 1))
    (swap! x y)
    (check (cons x y) => '(1 . 2))))

(define-syntax swap!
  (rsc-macro-transformer
    (lambda (form environment)
      (let ((a (cadr form))
            (b (caddr form))
            (x (make-syntactic-closure environment '() 'x))
            (let (make-syntactic-closure environment '() 'let))
            (set! (make-syntactic-closure environment '() 'set!)))
        `(,let ((,x ,a))
           (,set! ,a ,b)
           (,set! ,b ,x))))))

(let ((x 1)
      (y 2))
  (swap! x y)
  (check (cons x y) => '(2 . 1))
  (swap! x y)
  (check (cons x y) => '(1 . 2))
  (let ((a 'a)
        (b 'b)
        (let 'let)
        (set! 'set!))
    (swap! x y)
    (check (cons x y) => '(2 . 1))
    (swap! x y)
    (check (cons x y) => '(1 . 2))))

(define-syntax swap!
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((a (cadr form))
            (b (caddr form)))
       `(,(rename 'let) ((,(rename 'x) ,a))
          (,(rename 'set!) ,a ,b)
          (,(rename 'set!) ,b ,(rename 'x)))))))

(let ((x 1)
      (y 2))
  (swap! x y)
  (check (cons x y) => '(2 . 1))
  (swap! x y)
  (check (cons x y) => '(1 . 2))
  (let ((a 'a)
        (b 'b)
        (let 'let)
        (set! 'set!))
    (swap! x y)
    (check (cons x y) => '(2 . 1))
    (swap! x y)
    (check (cons x y) => '(1 . 2))))

(define-syntax swap!
  (syntax-rules ()
    ((swap! a b)
     (let ((x a))
       (set! a b)
       (set! b x)))))

(let ((x 1)
      (y 2))
  (swap! x y)
  (check (cons x y) => '(2 . 1))
  (swap! x y)
  (check (cons x y) => '(1 . 2))
  (let ((a 'a)
        (b 'b)
        (let 'let)
        (set! 'set!))
    (swap! x y)
    (check (cons x y) => '(2 . 1))
    (swap! x y)
    (check (cons x y) => '(1 . 2))))

; ---- LET-SYNTAX --------------------------------------------------------------

(check (let ((x 'outer))
         (let-syntax ((m (sc-macro-transformer
                           (lambda (form environment)
                             'x))))
           (let ((x 'inner))
             (m)))) => 'outer)

(check (let ((x 'outer))
         (let-syntax ((m (sc-macro-transformer
                           (lambda (form environment)
                             'x))))
           (let ((x 'inner-1))
             (let ((x 'inner-2))
               (let ((x 'inner-3))
                 (m)))))) => 'outer)

(check (let ((x 'outer))
         (let-syntax ((m (sc-macro-transformer
                           (lambda (form environment)
                             (let ((x 'transformer-1))
                               (let ((x 'transformer-2))
                                 (let ((x 'transformer-3))
                                   'x)))))))
           (let ((x 'inner))
             (m)))) => 'outer)

(check (let ((x 'outer))
         (let-syntax ((m (sc-macro-transformer
                           (lambda (form environment)
                             (let ((x 'transformer-1))
                               (let ((x 'transformer-2))
                                 (let ((x 'transformer-3))
                                   'x)))))))
           (let ((x 'inner-1))
             (let ((x 'inner-2))
               (let ((x 'inner-3))
                 (m)))))) => 'outer)

(check (let ((x 'outer))
         (let-syntax ((m (rsc-macro-transformer
                           (lambda (form environment)
                             (make-syntactic-closure environment '() 'x)))))
           (let ((x 'inner))
             (m)))) => 'outer)

(check (let ((x 'outer))
         (let-syntax ((m (rsc-macro-transformer
                           (lambda (form environment)
                             (make-syntactic-closure environment '() 'x)))))
           (let ((x 'inner-1))
             (let ((x 'inner-2))
               (let ((x 'inner-3))
                 (m)))))) => 'outer)

(check (let ((x 'outer))
         (let-syntax ((m (rsc-macro-transformer
                           (lambda (form environment)
                             (let ((x 'transformer-1))
                               (let ((x 'transformer-2))
                                 (let ((x 'transformer-3))
                                   (make-syntactic-closure environment '() 'x))))))))
           (let ((x 'inner))
             (m)))) => 'outer)

(check (let ((x 'outer))
         (let-syntax ((m (rsc-macro-transformer
                           (lambda (form environment)
                             (let ((x 'transformer-1))
                               (let ((x 'transformer-2))
                                 (let ((x 'transformer-3))
                                   (make-syntactic-closure environment '() 'x))))))))
           (let ((x 'inner-1))
             (let ((x 'inner-2))
               (let ((x 'inner-3))
                 (m)))))) => 'outer)

(check (let ((x 'outer))
         (let-syntax ((m (er-macro-transformer
                           (lambda (form rename compare)
                             (rename 'x)))))
           (let ((x 'inner))
             (m)))) => 'outer)

(check (let ((x 'outer))
         (let-syntax ((m (er-macro-transformer
                           (lambda (form rename compare)
                             (rename 'x)))))
           (let ((x 'inner-1))
             (let ((x 'inner-2))
               (let ((x 'inner-3))
                 (m)))))) => 'outer)

(check (let ((x 'outer))
         (let-syntax ((m (er-macro-transformer
                           (lambda (form rename compare)
                             (let ((x 'transformer-1))
                               (let ((x 'transformer-2))
                                 (let ((x 'transformer-3))
                                   (rename 'x))))))))
           (let ((x 'inner))
             (m)))) => 'outer)

(check (let ((x 'outer))
         (let-syntax ((m (er-macro-transformer
                           (lambda (form rename compare)
                             (let ((x 'transformer-1))
                               (let ((x 'transformer-2))
                                 (let ((x 'transformer-3))
                                   (rename 'x))))))))
           (let ((x 'inner-1))
             (let ((x 'inner-2))
               (let ((x 'inner-3))
                 (m)))))) => 'outer)

(check (let ((x 'outer))
         (let-syntax ((m (syntax-rules ()
                           ((m) x))))
           (let ((x 'inner))
             (m)))) => 'outer)

(check (let ((x 'outer))
         (let-syntax ((m (syntax-rules ()
                           ((m) x))))
           (let ((x 'inner-1))
             (let ((x 'inner-2))
               (let ((x 'inner-3))
                 (m)))))) => 'outer)

; ---- LETREC-SYNTAX -----------------------------------------------------------

(check (let ((x 'outer))
         (letrec-syntax ((m (sc-macro-transformer
                              (lambda (form environment)
                                'x))))
           (let ((x 'inner))
             (m)))) => 'outer)

(check (let ((x 'outer))
         (letrec-syntax ((m (sc-macro-transformer
                              (lambda (form environment)
                                'x))))
           (let ((x 'inner-1))
             (let ((x 'inner-2))
               (let ((x 'inner-3))
                 (m)))))) => 'outer)

(check (let ((x 'outer))
         (letrec-syntax ((m (sc-macro-transformer
                              (lambda (form environment)
                                (let ((x 'transformer-1))
                                  (let ((x 'transformer-2))
                                    (let ((x 'transformer-3))
                                      'x)))))))
           (let ((x 'inner))
             (m)))) => 'outer)

(check (let ((x 'outer))
         (letrec-syntax ((m (sc-macro-transformer
                              (lambda (form environment)
                                (let ((x 'transformer-1))
                                  (let ((x 'transformer-2))
                                    (let ((x 'transformer-3))
                                      'x)))))))
           (let ((x 'inner-1))
             (let ((x 'inner-2))
               (let ((x 'inner-3))
                 (m)))))) => 'outer)

(check (let ((x 'outer))
         (letrec-syntax ((m (rsc-macro-transformer
                              (lambda (form environment)
                                (make-syntactic-closure environment '() 'x)))))
           (let ((x 'inner))
             (m)))) => 'outer)

(check (let ((x 'outer))
         (letrec-syntax ((m (rsc-macro-transformer
                              (lambda (form environment)
                                (make-syntactic-closure environment '() 'x)))))
           (let ((x 'inner-1))
             (let ((x 'inner-2))
               (let ((x 'inner-3))
                 (m)))))) => 'outer)

(check (let ((x 'outer))
         (letrec-syntax ((m (rsc-macro-transformer
                              (lambda (form environment)
                                (let ((x 'transformer-1))
                                  (let ((x 'transformer-2))
                                    (let ((x 'transformer-3))
                                      (make-syntactic-closure environment '() 'x))))))))
           (let ((x 'inner))
             (m)))) => 'outer)

(check (let ((x 'outer))
         (letrec-syntax ((m (rsc-macro-transformer
                              (lambda (form environment)
                                (let ((x 'transformer-1))
                                  (let ((x 'transformer-2))
                                    (let ((x 'transformer-3))
                                      (make-syntactic-closure environment '() 'x))))))))
           (let ((x 'inner-1))
             (let ((x 'inner-2))
               (let ((x 'inner-3))
                 (m)))))) => 'outer)

(check (let ((x 'outer))
         (letrec-syntax ((m (er-macro-transformer
                              (lambda (form rename compare)
                                (rename 'x)))))
           (let ((x 'inner))
             (m)))) => 'outer)

(check (let ((x 'outer))
         (letrec-syntax ((m (er-macro-transformer
                              (lambda (form rename compare)
                                (rename 'x)))))
           (let ((x 'inner-1))
             (let ((x 'inner-2))
               (let ((x 'inner-3))
                 (m)))))) => 'outer)

(check (let ((x 'outer))
         (letrec-syntax ((m (er-macro-transformer
                              (lambda (form rename compare)
                                (let ((x 'transformer-1))
                                  (let ((x 'transformer-2))
                                    (let ((x 'transformer-3))
                                      (rename 'x))))))))
           (let ((x 'inner))
             (m)))) => 'outer)

(check (let ((x 'outer))
         (letrec-syntax ((m (er-macro-transformer
                              (lambda (form rename compare)
                                (let ((x 'transformer-1))
                                  (let ((x 'transformer-2))
                                    (let ((x 'transformer-3))
                                      (rename 'x))))))))
           (let ((x 'inner-1))
             (let ((x 'inner-2))
               (let ((x 'inner-3))
                 (m)))))) => 'outer)

(check (let ((x 'outer))
         (letrec-syntax ((m (syntax-rules ()
                              ((m) x))))
           (let ((x 'inner))
             (m)))) => 'outer)

(check (let ((x 'outer))
         (letrec-syntax ((m (syntax-rules ()
                              ((m) x))))
           (let ((x 'inner-1))
             (let ((x 'inner-2))
               (let ((x 'inner-3))
                 (m)))))) => 'outer)

(check ((lambda xs
          (letrec-syntax ((m (syntax-rules ()
                               ((m) xs))))
            (let ((x 'inner))
              (m))))
        'outer)
  => '(outer))

(check ((lambda xs
          (let ((x 'x))
            (letrec-syntax ((m (syntax-rules ()
                                 ((m) xs))))
              (let ((x 'inner))
                (m)))))
        'outer)
  => '(outer))

; ------------------------------------------------------------------------------

(define f
  (lambda xs
    (letrec-syntax ((m (syntax-rules ()
                         ((m) xs))))
      (m))))

(check (f 1 2 3) => '(1 2 3))

(define-syntax macro
  (syntax-rules ()
    ((macro)
     (lambda xs
       (letrec-syntax ((inner-macro (syntax-rules ()
                                      ((inner-macro) xs))))
         (inner-macro))))))

(check ((macro) 1 2 3) => '(1 2 3))

; ------------------------------------------------------------------------------

(define-syntax aif
  (sc-macro-transformer
    (lambda (form at-use)
      (let ((test (make-syntactic-closure at-use '() (cadr form)))
            (consequent (make-syntactic-closure at-use '(it) (caddr form)))
            (alternative (if (null? (cdddr form))
                             (if #f #f)
                             (make-syntactic-closure at-use '() (cadddr form)))))
        `(let ((it ,test))
           (if it ,consequent ,alternative))))))

(check (aif (memq 'b '(a b c))
            (car it))
  => 'b)

(check (aif (memq 'b '(a b c))
            (let ((it '(inner)))
              (car it)))
  => 'b)

(check (aif (memq 'b '(a b c))
            (let ((it '(inner-1)))
              (let ((it '(inner-0)))
                (car it))))
  => 'b)

(check (let ((it '(outer)))
         (aif (memq 'b '(a b c))
              (let ((it '(inner)))
                (car it))))
  => 'b)

; ------------------------------------------------------------------------------

(define-syntax LET
  (er-macro-transformer-v2
    (lambda (form rename compare)
      (if (identifier? (cadr form))
          (cons (rename 'letrec)
                (cons (list (list (cadr form)
                                  (cons (rename 'lambda)
                                        (cons (map car (caddr form))
                                              (cdddr form)))))
                      (list (cons (cadr form)
                                  (map cadr (caddr form))))))
          (cons (cons (rename 'lambda)
                      (cons (map car (cadr form))
                            (cddr form)))
                (map cadr (cadr form)))))))

(check (LET ((x 1)
             (y 2)
             (z 3))
         (+ x y z)) => 6)

(define-syntax LET*
  (er-macro-transformer-v2
    (lambda (form rename compare)
      (if (null? (cadr form))
          (cons (rename 'LET)
                (cons (list)
                      (cddr form)))
          (cons (rename 'LET)
                (cons (list (caadr form))
                      (list (cons (rename 'LET*)
                                  (cons (cdadr form)
                                        (cddr form))))))))))

(check (LET* ((x 1)
              (y (+ x 1))
              (z (+ y 1)))
         (+ x y z))
  => 6)

(define-syntax COND
  (er-macro-transformer-v2
    (lambda (form rename compare)
      (if (null? (cdr form))
          (if #f #f)
          ((lambda (clause)
             (if (compare (rename 'else)
                          (car clause))
                 (cons (rename 'begin)
                       (cdr clause))
                 (list (rename 'if)
                       (car clause)
                       (cons (rename 'begin)
                             (cdr clause))
                       (cons (rename 'cond)
                             (cddr form)))))
           (cadr form))))))

(define (f x)
  (define y 'two-or-more)
  (COND ((= x 0) 'zero)
        ((= x 1) 'one)
        (else y)))

(check (f 0) => 'zero)
(check (f 1) => 'one)
(check (f 2) => 'two-or-more)

(define (LENGTH x)
  (define something '())
  (LET loop ((x x)
             (n 0))
    (if (pair? x)
        (loop (cdr x)
              (+ n 1))
        n)))

(check (LENGTH '(a b c . d)) => 3)

; ------------------------------------------------------------------------------

(check-report)

(exit (check-passed? 58))
