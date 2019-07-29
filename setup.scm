; ------------------------------------------------------------------------------
;   Link Externals
; ------------------------------------------------------------------------------

(define fundamental.so (linker "./libmeevax-fundamental.so"))

(define car  (link fundamental.so "car"))
(define cdr  (link fundamental.so "cdr"))
(define cons (link fundamental.so "cons"))

(define numerical.so (linker "./libmeevax-numerical.so"))

(define *  (link numerical.so "multiplication"))
(define +  (link numerical.so "addition"))
(define -  (link numerical.so "subtraction"))
(define /  (link numerical.so "division"))
(define <  (link numerical.so "less"))
(define <= (link numerical.so "less_equal"))
(define >  (link numerical.so "greater"))
(define >= (link numerical.so "greater_equal"))

(define real? (link numerical.so "real_"))

(define experimental.so (linker "./libmeevax-experimental.so"))

(define display        (link experimental.so "display"))
(define emergency-exit (link experimental.so "emergency_exit"))
(define eq?            (link experimental.so "eq_"))
(define eqv?           (link experimental.so "eqv_"))
(define pair?          (link experimental.so "pair_"))


; ------------------------------------------------------------------------------
;   Setup CxR
; ------------------------------------------------------------------------------

(define caar (lambda (x) (car (car x))))
(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define cddr (lambda (x) (cdr (cdr x))))

; ------------------------------------------------------------------------------
;   Bootstrap Quasiquote
; ------------------------------------------------------------------------------

(define list
  (lambda xs xs))

(define list-tail
  (lambda (x k)
    (if (zero? k) x
        (list-tail (cdr x) (- k 1)))))

(define list-ref
  (lambda (list. k)
    (car (list-tail list. k))))

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

; TODO
; (define undefined
;   (lambda ()
;     (if #false #false)))

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

(define make-list
  (lambda (n o)
    (let ((default (if (pair? o) (car o))))
      (let rec ((n n)
                (result '()))
        (if (<= n 0) result
            (rec (- n 1)
                 (cons default result)))))))

(define list-copy
  (lambda (x)
    (let rec ((x x)
              (result '()))
      (if (pair? x)
          (rec (cdr x)
               (cons (car x) result))
          (append (reverse result) x)))))

(define member
  (lambda (o x . c)
    (let ((compare (if (pair? c) (car c) equal?)))
      (let rec ((x x))
        (and (pair? x)
             (if (compare o (car x)) x
                 (rec (cdr x))))))))

(define memq
  (lambda (o x)
    (member o x eq?)))

(define memv
  (lambda (o x)
    (member o x eqv?)))

(define assoc
  (lambda (o x . c)
    (let ((compare (if (pair? c) (car c) equal?)))
      (let assoc ((x x))
        (if (null? x) #false
            (if (compare o (caar x))
                (car x)
                (assoc (cdr x))))))))

(define assq
  (lambda (o x)
    (assoc o x eq?)))

(define assv
  (lambda (o x)
    (assoc o x eqv?)))

(define apply-1
  (lambda (proc args)
    (proc . args)))

; ; This cannot detect circular-list
; (define length
;   (lambda (list.)
;     (let loop ((list. list.)
;                (result 0))
;       (if (pair? list.)
;           (loop (cdr list.) (+ result 1))
;           result))))

(define length
  (lambda (x)
    (let loop ((x x)
               (lag x)
               (result 0))
      (if (pair? x)
          (let ((x (cdr x))
                (result (+ result 1)))
            (if (pair? x)
                (let ((x (cdr x))
                      (lag (cdr lag))
                      (result (+ result 1)))
                  (and (not (eq? x lag))
                       (loop x lag result)))
                result))
          result))))

(define apply
  (lambda (procedure x . xs)
    (if (null? xs)
        (apply-1 procedure x)
        (let ((reversed (reverse (cons x xs))))
          (apply-1 procedure (append-2 (reverse (cdr reversed)) (car reversed)))))))

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

(define positive?
  (lambda (n)
    (> n 0)))

(define negative?
  (lambda (n)
    (< n 0)))

; (define even?
;   (lambda (n)
;     (= (remainder n 2) 0)))

(define even?
  (lambda (n)
    (if (zero? n) #true
        (odd? (- n 1)))))

; (define odd?
;   (lambda (n)
;     (not (even? n))))

(define odd?
  (lambda (n)
    (if (zero? n) #false
        (even? (- n 1)))))

(define abs
  (lambda (n)
    (if (< n 0) (- n) n)))

; (define quotient truncate-quotient)
; (define remainder truncate-remainder)
; (define modulo floor-remainder)

; (define gcd-2
;   (lambda (a b)
;     (if (zero? b)
;         (abs a)
;         (gcd b (remainder a b)))))
;
; (define gcd
;   (lambda xs
;     (if (null? xs) 0
;         (let rec ((n (car xs))
;                   (ns (cdr xs)))
;           (if (null? ns) n
;               (rec (gcd-2 n (car ns)) (cdr ns)))))))
;
; (define lcm-2
;   (lambda (a b)
;     (abs (quotient (* a b) (gcd a b)))))
;
; (define lcm
;   (lambda xs
;     (if (null? xs) 1
;         (let rec ((n (car xs))
;                   (ns (cdr ns)))
;           (if (null? ns) n
;               (rec (lcm-2 n (car ns)) (cdr ns)))))))

(define boolean?
  (lambda (object)
    (if (or (eq? object #true)
            (eq? object #false))
      #true
      #false)))

(define newline
  (lambda ()
    (display "\n")))

(define swap!
  (macro (a b)
    (let ((x (make-symbol)))
     `(let ((,x ,a))
        (set! ,a ,b)
        (set! ,b ,x)))))

(define square
  (lambda (x)
    (* x x)))


; ------------------------------------------------------------------------------
;   Control Features
; ------------------------------------------------------------------------------

(define map-1
  (lambda (procedure x result)
    (if (pair? x)
        (map-1 procedure
               (cdr x)
               (cons (procedure (car x)) result))
        (reverse result))))

(define map-n
  (lambda (procedure xs result)
    (if (every pair? xs)
        (map-n procedure
               (map-1 cdr xs '())
               (cons (apply procedure (map-1 car xs '())) result))
        (reverse result))))

(define map
  (lambda (procedure x . xs)
    (if (null? xs)
        (map-1 procedure x '())
        (map-n procedure (cons x xs) '()))))

(define for-each-1
  (lambda (f x)
    (if (pair? x)
        (begin (f (car x))
               (for-each-1 f (cdr x))))))

(define for-each
  (lambda (f x . xs)
    (if (null? xs)
        (for-each-1 f x)
        (begin (apply map f x xs)
               undefined))))

(define any-1
  (lambda (predicate x)
    (if (pair? (cdr x))
        (let ((result (predicate (car x))))
          (if result
              result
              (any-1 predicate (cdr x))))
        (predicate (car x)))))

(define any-n
  (lambda (predicate xs)
    (if (every pair? xs)
        (let ((result (apply predicate (map car xs))))
          (if result
              result
              (any-n predicate (map cdr xs))))
        #false)))

(define any
  (lambda (predicate x . xs)
    (if (null? xs)
        (if (pair? x)
            (any-1 predicate x)
            #false)
        (any-n predicate (cons x xs)))))

(define every-1
  (lambda (predicate x)
    (if (null? (cdr x))
        (predicate (car x))
        (if (predicate (car x))
            (every-1 predicate (cdr x))
            #false))))

(define every
  (lambda (predicate x . xs)
    (if (null? xs)
        (if (pair? x)
            (every-1 predicate x)
            #true)
        (not (apply any (lambda xs (not (apply predicate xs))) x xs)))))

; ------------------------------------------------------------------------------
;   Values
; ------------------------------------------------------------------------------

(define values
  (lambda xs
    (call-with-current-continuation
      (lambda (continuation)
        (apply continuation xs)))))

(define call-with-values
  (lambda (producer consumer)
    (let ((result (producer)))
      (if (and (pair? result)
               (eq? (car result) 'values))
          (apply consumer (cdr result))
          (consumer result)))))


