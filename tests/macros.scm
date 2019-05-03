(define list (lambda e e))

(define unit? (lambda (x) (eq? x '())))
(define Unit? (syntax (x) (list 'eq? x ''())))

(define quasiquote (syntax x (quasiquote-aux x)))

(define quasiquote-aux
  (lambda (Exp)
    (if (pair? Exp)
        (if (pair? (car Exp))
            (if (eq? (caar Exp) 'unquote)
                (list 'cons (cadar Exp) (quasiquote-aux (cdr Exp)))
                (if (eq? (caar Exp) 'unquote-splicing)
                    (list 'append (cadar Exp) (quasiquote-aux (cdr Exp)))
                    (list 'cons (quasiquote-aux (car Exp)) (quasiquote-aux (cdr Exp)))))
            (list 'cons (list 'quote (car Exp)) (quasiquote-aux (cdr Exp))))
        (list 'quote Exp))))

(quasiquote-aux '(a b c)); => (cons 'a (cons 'b (cons 'c '())))
(quasiquote-aux '(,a b c)); => (cons a (cons 'b (cons 'c '())))
(quasiquote-aux '(,a ,@b c)); => (cons a (append b (cons 'c '())))
(quasiquote-aux '(,(car a) ,@(cdr b) c)); => (cons (car a) (append (cdr b) (cons 'c '())))

