(import (scheme base)
        (scheme cxr)
        (scheme process-context)
        (scheme write)
        (srfi 78)
        (only (srfi 211 syntactic-closures) make-syntactic-closure rsc-macro-transformer sc-macro-transformer)
        (only (srfi 211 explicit-renaming) er-macro-transformer))

(define (print . xs)
  (for-each (lambda (x)
              (display x))
            xs)
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

; ------------------------------------------------------------------------------

(check-report)

(exit (check-passed? 44))
