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
  (lambda (object)
    (eq? object '())))

(define append
  (lambda (xs ys)
    (if (null? xs)
        ys
        (cons (car xs)
              (append (cdr xs) ys)))))

(define and
  (macro <tests>
    (if (null? <tests>) #true
    (if (null? (cdr <tests>)) #true
    (if (null? (cddr <tests>))
        (cadr <tests>)
        (list (list 'lambda (list 'head 'thunk)
                (list 'if 'head
                          (list 'thunk)
                          'head))
              (cadr <tests>)
              (list 'lambda '()
                (append (list 'and)
                        (cddr <tests>)))))))))

(define or
  (macro <tests>
    (if (null? <tests>) #false
    (if (null? (cdr <tests>)) #false
    (if (null? (cddr <tests>))
        (cadr <tests>)
        (list (list 'lambda (list 'head 'thunk)
                (list 'if 'head
                          'head
                          (list 'thunk)))
              (cadr <tests>)
              (list 'lambda '()
                (append (list 'or)
                        (cddr <tests>)))))))))

(define not
  (lambda (test)
    (if test #false #true)))

(define boolean?
  (lambda (object)
    (if (or (eq? object #true)
            (eq? object #false))
      #true
      #false)))

(define quasiquote-expand
  (lambda (e depth)
    (if (not (pair? e))
        (list 'quote e)
        (if (eq? (car e) 'quasiquote)
            (list 'cons 'quasiquote (quasiquote-expand (cdr e) (+ depth 1)))
            (if (eq? (car e) 'unquote)
                (if (< 0 depth)
                    (list 'cons 'unquote (quasiquote-expand (cdr e) (- depth 1)))
                    (if (and (not (null? (cdr e))) (null? (cddr e)))
                        (cadr e)
                        (error "illegal unquote")))
                (if (eq? (car e) 'unquote-splicing)
                    (if (< 0 depth)
                        (list 'cons 'unquote-splicing (quasiquote-expand (cdr e) (- depth 1)))
                        (error "illegal unquote-splicing"))
                    (list 'append (quasiquote-expand-list (car e) depth)
                                  (quasiquote-expand      (cdr e) depth))))))))

(define quasiquote-expand-list
  (lambda (e depth)
    (if (not (pair? e))
        (list 'quote (list e))
        (if (eq? (car e) 'quasiquote)
            (list 'list (list 'cons 'quasiquote (quasiquote-expand (cdr e) (+ depth 1))))
            (if (eq? (car e) 'unquote)
                (if (< 0 depth)
                    (list 'list (list 'cons 'unquote (quasiquote-expand (cdr e) (- depth 1))))
                    (cons 'list (cdr e)))
                (if (eq? (car e) 'unquote-splicing)
                    (if (< 0 depth)
                        (list 'list (list 'cons 'unquote-splicing (quasiquote-expand (cdr e) (- depth 1))))
                        (cons 'append (cdr e)))
                    (list 'list (list 'append (quasiquote-expand-list (car e) depth)
                                              (quasiquote-expand      (cdr e) depth)))))))))

(define quasiquote
  (macro (<template>)
    (quasiquote-expand <template> 0)))

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

