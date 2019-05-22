(define libmeevax-base.so "./libscheme-base.so")

(define eq?   (link-procedure libmeevax-base.so "symbolic_equal"))
(define pair? (link-procedure libmeevax-base.so "is_pair"))

(define + (link-procedure libmeevax-base.so "plus"))
(define * (link-procedure libmeevax-base.so "multiplies"))
(define - (link-procedure libmeevax-base.so "minus"))
(define / (link-procedure libmeevax-base.so "divides"))

(define (scheme write) "./libmeevax-write.so"); HACK

;; cxr

(define caar (lambda (e) (car (car e))))
(define cadr (lambda (e) (car (cdr e))))
(define cdar (lambda (e) (cdr (car e))))
(define caar (lambda (e) (car (car e))))

(define caaar (lambda (e) (car (car (car e)))))
(define caadr (lambda (e) (car (car (cdr e)))))
(define cadar (lambda (e) (car (cdr (car e)))))
(define caddr (lambda (e) (car (cdr (cdr e)))))
(define cdaar (lambda (e) (cdr (car (car e)))))
(define cdadr (lambda (e) (cdr (car (cdr e)))))
(define cddar (lambda (e) (cdr (cdr (car e)))))
(define cdddr (lambda (e) (cdr (cdr (cdr e)))))

(define caaaar (lambda (e) (car (car (car (car e))))))
(define caaadr (lambda (e) (car (car (car (cdr e))))))
(define caadar (lambda (e) (car (car (cdr (car e))))))
(define caaddr (lambda (e) (car (car (cdr (cdr e))))))
(define cadaar (lambda (e) (car (cdr (car (car e))))))
(define cadadr (lambda (e) (car (cdr (car (cdr e))))))
(define caddar (lambda (e) (car (cdr (cdr (car e))))))
(define cadddr (lambda (e) (car (cdr (cdr (cdr e))))))
(define cdaaar (lambda (e) (cdr (car (car (car e))))))
(define cdaadr (lambda (e) (cdr (car (car (cdr e))))))
(define cdadar (lambda (e) (cdr (car (cdr (car e))))))
(define cdaddr (lambda (e) (cdr (car (cdr (cdr e))))))
(define cddaar (lambda (e) (cdr (cdr (car (car e))))))
(define cddadr (lambda (e) (cdr (cdr (car (cdr e))))))
(define cdddar (lambda (e) (cdr (cdr (cdr (car e))))))
(define cddddr (lambda (e) (cdr (cdr (cdr (cdr e))))))

(define list
  (lambda xs xs))

(define null?
  (lambda (e)
    (eq? e '())))

(define not
  (lambda (e)
    (if e #false #true)))

(define append
  (lambda (xs ys)
    (if (null? xs)
        ys
        (cons (car xs)
              (append (cdr xs) ys)))))

(define transform
  (lambda (e)
    (if (not (pair? e))
        (list 'quote e)
        (if (not (pair? (car e)))
            (list 'cons (list 'quote (car e)) (transform (cdr e)))
            (if (eq? (caar e) 'unquote)
                (list 'cons (cadar e) (transform (cdr e)))
                (if (eq? (caar e) 'unquote-splicing)
                    (list 'append (cadar e) (transform (cdr e)))
                    (list 'cons (transform (car e))
                                (transform (cdr e)))))))))

; (transform '(a b c)); => (cons 'a (cons 'b (cons 'c '())))
; (transform '(,a b c)); => (cons a (cons 'b (cons 'c '())))
; (transform '(,a ,@b c)); => (cons a (append b (cons 'c '())))
; (transform '(,(car a) ,@(cdr b) c)); => (cons (car a) (append (cdr b) (cons 'c '())))

(define quasiquote
  (syntax (e)
    (transform e)))

(define a '(1 2 3))

`(a b c); => (a b c)
`(,a b c); => ((1 2 3) b c)
`(,@a b c); => (1 2 3 b c)

