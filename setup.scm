(define dlopen dynamic-link-open)
(define dlsym dynamic-link-procedure)

(define libmeevax-numerical.so (dlopen "./libmeevax-numerical.so"))

(define * (dlsym libmeevax-numerical.so "multiplication"))
(define + (dlsym libmeevax-numerical.so "addition"))
(define - (dlsym libmeevax-numerical.so "subtraction"))
(define / (dlsym libmeevax-numerical.so "division"))
(define < (dlsym libmeevax-numerical.so "less"))
(define > (dlsym libmeevax-numerical.so "greater"))

(define libmeevax-experimental.so (dlopen "./libmeevax-experimental.so"))

(define display (dlsym libmeevax-experimental.so "display"))
(define emergency-exit (dlsym libmeevax-experimental.so "emergency_exit"))
(define eq? (dlsym libmeevax-experimental.so "addressive_equals"))
(define eqv? (dlsym libmeevax-experimental.so "semantic_equals"))
(define pair? (dlsym libmeevax-experimental.so "is_pair"))

(define λ lambda)

; HACK
(define car
  (λ (e)
    (car e)))

; HACK
(define cdr
  (λ (e)
    (cdr e)))

;; cxr

(define caar (λ (e) (car (car e))))
(define cadr (λ (e) (car (cdr e))))
(define cdar (λ (e) (cdr (car e))))
(define cddr (λ (e) (cdr (cdr e))))

; (define caaar (λ (e) (car (car (car e)))))
; (define caadr (λ (e) (car (car (cdr e)))))
; (define cadar (λ (e) (car (cdr (car e)))))
; (define caddr (λ (e) (car (cdr (cdr e)))))
; (define cdaar (λ (e) (cdr (car (car e)))))
; (define cdadr (λ (e) (cdr (car (cdr e)))))
; (define cddar (λ (e) (cdr (cdr (car e)))))
; (define cdddr (λ (e) (cdr (cdr (cdr e)))))
;
; (define caaaar (λ (e) (car (car (car (car e))))))
; (define caaadr (λ (e) (car (car (car (cdr e))))))
; (define caadar (λ (e) (car (car (cdr (car e))))))
; (define caaddr (λ (e) (car (car (cdr (cdr e))))))
; (define cadaar (λ (e) (car (cdr (car (car e))))))
; (define cadadr (λ (e) (car (cdr (car (cdr e))))))
; (define caddar (λ (e) (car (cdr (cdr (car e))))))
; (define cadddr (λ (e) (car (cdr (cdr (cdr e))))))
; (define cdaaar (λ (e) (cdr (car (car (car e))))))
; (define cdaadr (λ (e) (cdr (car (car (cdr e))))))
; (define cdadar (λ (e) (cdr (car (cdr (car e))))))
; (define cdaddr (λ (e) (cdr (car (cdr (cdr e))))))
; (define cddaar (λ (e) (cdr (cdr (car (car e))))))
; (define cddadr (λ (e) (cdr (cdr (car (cdr e))))))
; (define cdddar (λ (e) (cdr (cdr (cdr (car e))))))
; (define cddddr (λ (e) (cdr (cdr (cdr (cdr e))))))

(define list
  (λ xs xs))

(define null?
  (λ (object)
    (eq? object '())))

(define append-2
  (λ (list.1 list.2)
    (if (null? list.1) list.2
        (cons (car list.1)
              (append-2 (cdr list.1) list.2)))))

;; simple reverse (but slow)
(define reverse
  (λ (list.)
    (if (null? list.)
       '()
        (append-2 (reverse (cdr list.))
                  (list (car list.))))))

(define append-aux
  (λ (list.1 list.2)
    (if (null? list.1) list.2
        (append-aux (cdr list.1)
                    (append-2 (car list.1) list.2)))))

(define append
  (λ lists
    (if (null? lists)
       '()
        ((λ (reversed)
           (append-aux (cdr reversed)
                       (car reversed)))
         (reverse lists)))))

(define and
  (macro <tests>
    (if (null? <tests>) #true
    (if (null? (cdr <tests>)) #true
    (if (null? (cddr <tests>))
        (cadr <tests>)
        (list (list 'λ (list 'head 'thunk)
                (list 'if 'head
                          (list 'thunk)
                          'head))
              (cadr <tests>)
              (list 'λ '()
                (append (list 'and)
                        (cddr <tests>)))))))))

(define or
  (macro <tests>
    (if (null? <tests>) #false
    (if (null? (cdr <tests>)) #false
    (if (null? (cddr <tests>))
        (cadr <tests>)
        (list (list 'λ (list 'head 'thunk)
                (list 'if 'head
                          'head
                          (list 'thunk)))
              (cadr <tests>)
              (list 'λ '()
                (append (list 'or)
                        (cddr <tests>)))))))))

(define equal?
  (λ (object.1 object.2)
    (if (and (pair? object.1)
             (pair? object.2))
        (and (equal? (car object.1) (car object.2))
             (equal? (cdr object.1) (cdr object.2)))
        (eqv? object.1 object.2))))

(define not
  (λ (test)
    (if test #false #true)))

(define boolean?
  (λ (object)
    (if (or (eq? object #true)
            (eq? object #false))
      #true
      #false)))

(define quasiquote-expand
  (λ (e depth)
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
  (λ (e depth)
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
    (list 'cdr (list 'λ '() '()))))

(define interaction-environment
  (macro ()
    (list 'cdr (list 'macro '() '()))))

(define rename
  (λ (x)
    (λ () x)))

(define apply
  (λ (proc args)
    (proc . args)))

(define list-copy
  (λ (list.)
    (append-2 list. '())))

(define pair-copy-shallow
  (λ (pair)
    (cons (car pair) (cdr pair))))

(define pair-copy-deep
  (λ (object)
    (if (not (pair? object)) object
        (cons (pair-copy-deep (car object))
              (pair-copy-deep (cdr object))))))

(define when
  (macro (<test> . <expression>)
   `(if ,<test> (begin ,@<expression>))))

(define unless
  (macro (<test> . <expression>)
   `(if (not ,<test>) (begin ,@<expression>))))

(define map
  (λ (callee list.)
    (if (null? list.)
       '()
        (cons (callee (car list.))
              (map callee (cdr list.))))))

