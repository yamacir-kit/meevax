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

(define append-2
  (lambda (list.1 list.2)
    (if (null? list.1) list.2
        (cons (car list.1)
              (append-2 (cdr list.1) list.2)))))

;; simple reverse (but slow)
(define reverse
  (lambda (list.)
    (if (null? list.)
       '()
        (append-2 (reverse (cdr list.))
                  (list (car list.))))))

(define append-aux
  (lambda (list.1 list.2)
    (if (null? list.1) list.2
        (append-aux (cdr list.1)
                    (append-2 (car list.1) list.2)))))

(define append
  (lambda lists
    (if (null? lists)
       '()
        ((lambda (reversed)
           (append-aux (cdr reversed)
                       (car reversed)))
         (reverse lists)))))

(define and
  (macro <tests>
    (if (null? <tests>) #true
    (if (null? (cdr <tests>))
        (car <tests>)
        (list (list 'lambda (list 'head 'thunk)
                (list 'if 'head
                          (list 'thunk)
                          'head))
              (car <tests>)
              (list 'lambda '()
                (append (list 'and)
                        (cdr <tests>))))))))

(define or
  (macro <tests>
    (if (null? <tests>) #false
    (if (null? (cdr <tests>))
        (car <tests>)
        (list (list 'lambda (list 'head 'thunk)
                (list 'if 'head
                          'head
                          (list 'thunk)))
              (car <tests>)
              (list 'lambda '()
                (append (list 'or)
                        (cdr <tests>))))))))

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

(define map
  (lambda (callee list.)
    (if (null? list.)
       '()
        (cons (callee (car list.))
              (map callee (cdr list.))))))

(define map-2
  (lambda (callee list.1 list.2)
    (if (null? list.1)
       '()
        (cons (callee (car list.1) (car list.2))
              (map-2 callee (cdr list.1) (cdr list.2))))))

(define undefined)

(define letrec
  (macro (bindings . body)
    (let ((identifiers (map car bindings))
          (expressions (map cadr bindings)))
     `(let ,(map (lambda (x) `(,x ,undefined)) identifiers)
        ,@(map-2 (lambda (x y) `(set! ,x ,y)) identifiers expressions)
        ,@body))))

(define let
  (macro (bindings . body)
    (if (pair? bindings)
       `((lambda ,(map car bindings) ,@body) ,@(map cadr bindings))
       `(letrec ((,bindings (lambda ,(map car (car body)) ,@(cdr body))))
          (,bindings ,@(map cadr (car body)))))))

(define let*
  (macro (<specs> . <body>)
    (if (or (null? <specs>)
            (null? (cdr <specs>)))
       `(let (,(car <specs>)) ,@<body>)
       `(let (,(car <specs>)) (let* ,(cdr <specs>) ,@<body>)))))

(define current-lexical-environment
  (macro ()
    (list 'cdr (list 'lambda '() '()))))

(define interaction-environment
  (macro ()
    (list 'cdr (list 'macro '() '()))))

(define rename
  (lambda (x)
    (lambda () x)))

(define apply
  (lambda (proc args)
    (proc . args)))

(define list-copy
  (lambda (list.)
    (append-2 list. '())))

(define pair-copy-shallow
  (lambda (pair)
    (cons (car pair) (cdr pair))))

(define pair-copy-deep
  (lambda (object)
    (if (not (pair? object)) object
        (cons (pair-copy-deep (car object))
              (pair-copy-deep (cdr object))))))

(define when
  (macro (<test> . <expression>)
   `(if ,<test> (begin ,@<expression>))))

(define unless
  (macro (<test> . <expression>)
   `(if (not ,<test>) (begin ,@<expression>))))

(define equal?
  (lambda (object.1 object.2)
    (if (and (pair? object.1)
             (pair? object.2))
        (and (equal? (car object.1) (car object.2))
             (equal? (cdr object.1) (cdr object.2)))
        (eqv? object.1 object.2))))

(define = eqv?)

(define zero?
  (lambda (n)
    (= n 0)))

