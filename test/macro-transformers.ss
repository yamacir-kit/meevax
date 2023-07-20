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
            (let ((it 'inner))
              (car it)))
  => 'b)

(check (aif (memq 'b '(a b c))
            (let ((it 'inner-1))
              (let ((it 'inner-0))
                (car it))))
  => 'b)

(check (let ((it 'outer))
         (aif (memq 'b '(a b c))
              (let ((it 'inner))
                (car it))))
  => 'b)

; ------------------------------------------------------------------------------

(check-report)

(exit (check-passed? 52))
