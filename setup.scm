; ------------------------------------------------------------------------------
;   Link Externals
; ------------------------------------------------------------------------------

(define dlopen dynamic-link-open)
(define dlsym dynamic-link-procedure)

(define fundamental.so (dlopen "./libmeevax-fundamental.so"))

(define car  (dlsym fundamental.so "car"))
(define cdr  (dlsym fundamental.so "cdr"))
(define cons (dlsym fundamental.so "cons"))

(define numerical.so (dlopen "./libmeevax-numerical.so"))

(define * (dlsym numerical.so "multiplication"))
(define + (dlsym numerical.so "addition"))
(define - (dlsym numerical.so "subtraction"))
(define / (dlsym numerical.so "division"))
(define < (dlsym numerical.so "less"))
(define > (dlsym numerical.so "greater"))

(define experimental.so (dlopen "./libmeevax-experimental.so"))

(define display        (dlsym experimental.so "display"))
(define emergency-exit (dlsym experimental.so "emergency_exit"))
(define eq?            (dlsym experimental.so "addressive_equals"))
(define eqv?           (dlsym experimental.so "semantic_equals"))
(define pair?          (dlsym experimental.so "is_pair"))


; ------------------------------------------------------------------------------
;   Setup CxR
; ------------------------------------------------------------------------------

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


; ------------------------------------------------------------------------------
;   Bootstrap Quasiquote
; ------------------------------------------------------------------------------

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

; simple reverse (but slow)
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


; ------------------------------------------------------------------------------
;   Bootstrap Binding Constuctors
; ------------------------------------------------------------------------------

(define map-1
  (lambda (callee list.)
    (if (null? list.)
       '()
        (cons (callee (car list.))
              (map-1 callee (cdr list.))))))

(define map map-1) ; temporary (for unnamed-let)

(define unnamed-let
  (macro (bindings . body)
   `((lambda ,(map car bindings) ,@body) ,@(map cadr bindings))))

(define undefined) ; hacking

(define let unnamed-let) ; temporary (for letrec)

(define letrec*
  (macro (bindings . body)
    (let ((identifiers (map car bindings)))
     `(let ,(map (lambda (e) `(,e ,undefined)) identifiers)
        ,@(map (lambda (e) `(set! ,(car e) ,(cadr e))) bindings)
        ,@body))))

(define letrec letrec*) ; this is not currect

(define let
  (macro (bindings . body)
    (if (pair? bindings)
       `(unnamed-let ,bindings ,@body)
       `(letrec ((,bindings (lambda ,(map car (car body)) ,@(cdr body))))
          (,bindings ,@(map cadr (car body)))))))

(define let*
  (macro (<specs> . <body>)
    (if (or (null? <specs>)
            (null? (cdr <specs>)))
       `(let (,(car <specs>)) ,@<body>)
       `(let (,(car <specs>)) (let* ,(cdr <specs>) ,@<body>)))))


; ------------------------------------------------------------------------------
;   Bootstrap Other Derived Expression Types
; ------------------------------------------------------------------------------

(define else #true)

(define cond
  (macro clauses
    (if (null? clauses) undefined
        (if (eq? (caar clauses) 'else)
           `(begin ,@(cdar clauses))
            (if (null? (cdar clauses))
               `(let ((TEST ,(caar clauses)))
                  (if TEST TEST (cond ,@(cdr clauses))))
               `(if ,(caar clauses)
                    (begin ,@(cdar clauses))
                    (cond ,@(cdr clauses))))))))

(define memv
  (lambda (object list.)
    (if (null? list.) #false
        (if (eqv? object (car list.)) list.
            (memv object (cdr list.))))))

(define case
  (macro (key . clauses)
    (if (null? clauses) 'undefined
        (if (eq? (caar clauses) 'else)
           `(begin ,@(cdar clauses))
           `(if (memv ,key ',(caar clauses))
                (begin ,@(cdar clauses))
                (case ,key ,@(cdr clauses)))))))

; ------------------------------------------------------------------------------
;   Miscellaneous
; ------------------------------------------------------------------------------

(define call/cc call-with-current-continuation)

; (define current-lexical-environment
;   (macro ()
;     (list 'cdr (list 'lambda '() '()))))
;
; (define interaction-environment
;   (macro ()
;     (list 'cdr (list 'macro '() '()))))
;
; (define rename
;   (lambda (x)
;     (lambda () x)))


(define list-copy ; from srfi-1
  (lambda (list.)
    (let rec ((list. list.))
      (if (pair? list.)
          (cons (car list.) (rec (cdr list.)))
          list.))))

(define apply-1
  (lambda (proc args)
    (proc . args)))

(define make-operands
  (lambda (list.1 list.2)
    (if (null? list.2) list.1
        (make-operands (cons (car list.2) list.1) (cdr list.2)))))

; ; This cannot detect circular-list
; (define length
;   (lambda (list.)
;     (let loop ((list. list.)
;                (result 0))
;       (if (pair? list.)
;           (loop (cdr list.) (+ result 1))
;           result))))

(define length
  (lambda (list.)
    (let loop ((list. list.)
               (lag list.)
               (result 0))
      (if (pair? list.)
          (let ((list. (cdr list.))
                (result (+ result 1)))
            (if (pair? list.)
                (let ((list. (cdr list.))
                      (lag (cdr lag))
                      (result (+ result 1)))
                  (and (not (eq? list. lag))
                       (loop list. lag result)))
                result))
          result))))

(define apply
  (lambda (procedure . list.)
    (if (= (length list.) 1)
        (apply-1 procedure (car list.))
        (let ((reversed (reverse list.)))
          (apply-1 procedure (make-operands (car reversed) (cdr reversed)))))))

; (define pair-copy-shallow
;   (lambda (pair)
;     (cons (car pair) (cdr pair))))
;
; (define pair-copy-deep
;   (lambda (object)
;     (if (not (pair? object)) object
;         (cons (pair-copy-deep (car object))
;               (pair-copy-deep (cdr object))))))

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

(define boolean?
  (lambda (object)
    (if (or (eq? object #true)
            (eq? object #false))
      #true
      #false)))

(define newline
  (lambda ()
    (display "\n")))

; (define swap!
;   (macro (a b)
;    `(let ((x ,a))
;       (set! ,a ,b)
;       (set! ,b x))))

(define swap!
  (macro (a b)
    (let ((x (make-symbol)))
     `(let ((,x ,a))
        (set! ,a ,b)
        (set! ,b ,x)))))

(define square
  (lambda (x)
    (* x x)))

