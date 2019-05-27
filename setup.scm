(define libmeevax-base.so (dynamic-link-open "./libscheme-base.so"))

(define eq?   (dynamic-link-procedure libmeevax-base.so "symbolic_equal"))
(define pair? (dynamic-link-procedure libmeevax-base.so "is_pair"))

(define libmeevax-numerical.so (dynamic-link-open "./libmeevax-numerical.so"))

(define + (dynamic-link-procedure libmeevax-numerical.so "addition"))
(define * (dynamic-link-procedure libmeevax-numerical.so "multiplication"))
(define - (dynamic-link-procedure libmeevax-numerical.so "subtraction"))
(define / (dynamic-link-procedure libmeevax-numerical.so "division"))

(define < (dynamic-link-procedure libmeevax-numerical.so "less"))

; HACK
(define car
  (lambda (e)
    (car e)))

; HACK
(define cdr
  (lambda (e)
    (cdr e)))

;; cxr

(define caar (lambda (e) (car (car e))))
(define cadr (lambda (e) (car (cdr e))))
(define cdar (lambda (e) (cdr (car e))))
(define cddr (lambda (e) (cdr (cdr e))))

; (define caaar (lambda (e) (car (car (car e)))))
; (define caadr (lambda (e) (car (car (cdr e)))))
; (define cadar (lambda (e) (car (cdr (car e)))))
; (define caddr (lambda (e) (car (cdr (cdr e)))))
; (define cdaar (lambda (e) (cdr (car (car e)))))
; (define cdadr (lambda (e) (cdr (car (cdr e)))))
; (define cddar (lambda (e) (cdr (cdr (car e)))))
; (define cdddr (lambda (e) (cdr (cdr (cdr e)))))
;
; (define caaaar (lambda (e) (car (car (car (car e))))))
; (define caaadr (lambda (e) (car (car (car (cdr e))))))
; (define caadar (lambda (e) (car (car (cdr (car e))))))
; (define caaddr (lambda (e) (car (car (cdr (cdr e))))))
; (define cadaar (lambda (e) (car (cdr (car (car e))))))
; (define cadadr (lambda (e) (car (cdr (car (cdr e))))))
; (define caddar (lambda (e) (car (cdr (cdr (car e))))))
; (define cadddr (lambda (e) (car (cdr (cdr (cdr e))))))
; (define cdaaar (lambda (e) (cdr (car (car (car e))))))
; (define cdaadr (lambda (e) (cdr (car (car (cdr e))))))
; (define cdadar (lambda (e) (cdr (car (cdr (car e))))))
; (define cdaddr (lambda (e) (cdr (car (cdr (cdr e))))))
; (define cddaar (lambda (e) (cdr (cdr (car (car e))))))
; (define cddadr (lambda (e) (cdr (cdr (car (cdr e))))))
; (define cdddar (lambda (e) (cdr (cdr (cdr (car e))))))
; (define cddddr (lambda (e) (cdr (cdr (cdr (cdr e))))))

(define list
  (lambda xs xs))

(define null?
  (lambda (e)
    (eq? e '())))

(define append
  (lambda (xs ys)
    (if (null? xs)
        ys
        (cons (car xs)
              (append (cdr xs) ys)))))

(define and
  (macro e
    (if (null? e) #true
    (if (null? (cdr e)) #true
    (if (null? (cddr e))
        (cadr e)
        (list (list 'lambda (list head thunk)
                (list 'if 'head
                          (list 'thunk)
                          'head))
              (cadr e)
              (list 'lambda '()
                (append (list 'and)
                        (cddr e)))))))))

(define or
  (macro e
    (if (null? e) #false
    (if (null? (cdr e)) #false
    (if (null? (cddr e))
        (cadr e)
        (list (list 'lambda (list head thunk)
                (list 'if 'head
                          'head
                          (list 'thunk)))
              (cadr e)
              (list 'lambda '()
                (append (list 'or)
                        (cddr e)))))))))

(define not
  (lambda (e)
    (if e #false #true)))

(define quasiquote-expand
  (lambda (e depth)
    (if (not (pair? e)) `',e
        (if (eq? (car e) 'quasiquote)
            `(cons 'quasiquote ,(quasiquote-expand (cdr e) (+ depth 1)))
            (if (or (eq? (car e) 'unquote)
                    (eq? (car e) 'unquote-splicing))
                (if (< 0 depth)
                    `(cons ',(car e) ,(quasiquote-expand (cdr e) (- depth 1)))
                    (if (and (eq? (car e) 'unquote)
                             (not (null? (cdr e)))
                             (null? (cddr e)))
                        (cadr e)
                        (error "illegal")))
                `(append ,(quasiquote-expand-list (car e) depth)
                         ,(quasiquote-expand      (cdr e) depth)))))))

(define quasiquote-expand-list
  (lambda (e depth)
    (if (not (pair? e)) `'(,e)
        (if (eq? (car e) 'quasiquote)
            `(list (cons 'quasiquote)
                   ,(quasiquote-expand (cdr e) (+ depth 1)))
            (if (or (eq? (car e) 'unquote)
                    (eq? (car e) 'unquote-splicing))
                (if (< 0 depth)
                    `(list ',(car e) ,(quasiquote-expand (cdr e) (- depth 1)))
                    (if (eq? (car e) 'unquote)
                        `(list . ,(cdr e))
                        `(append . ,(cdr e))))
                `(list (append ,(quasiquote-expand-list (car e) depth)
                               ,(quasiquote-expand      (cdr e) depth))))))))

(define quasiquote
  (macro e
    (quasiquote-expand e 0)))

; (define qq-expand
;   (lambda (e)
;     (if (not (pair? e))
;         (list 'quote e)
;         (if (not (pair? (car e)))
;             (list 'cons (list 'quote (car e)) (qq-expand (cdr e)))
;             (if (eq? (caar e) 'unquote)
;                 (list 'cons (cadar e) (qq-expand (cdr e)))
;                 (if (eq? (caar e) 'unquote-splicing)
;                     (list 'append (cadar e) (qq-expand (cdr e)))
;                     (list 'cons (qq-expand (car e))
;                                 (qq-expand (cdr e)))))))))
;
; (define quasiquote
;   (macro (e)
;     (qq-expand e)))

(define current-lexical-environment
  (macro ()
    (list 'cdr (list 'lambda '() '()))))

(define current-global-environment
  (macro ()
    (list 'cdr (list 'macro '() '()))))

(define rename
  (lambda (x)
    (lambda () x)))

(define apply
  (lambda (proc args)
    (proc . args)))

