; ------------------------------------------------------------------------------
;   Link Externals
; ------------------------------------------------------------------------------

(define experimental.so
  (linker "./lib/libmeevax-experimental.so"))

(define display        (link experimental.so "display"))
(define emergency-exit (link experimental.so "emergency_exit"))

(define file-system.so
  (linker "./lib/libmeevax-file-system.so"))

(define input-file? (link file-system.so "input_file_"))
(define output-file? (link file-system.so "output_file_"))

(define open-input-file (link file-system.so "open_input_file"))
(define open-output-file (link file-system.so "open_output_file"))

(define close-input-file (link file-system.so "close_input_file"))
(define close-output-file (link file-system.so "close_output_file"))

; ------------------------------------------------------------------------------
;   Miscellaneous
; ------------------------------------------------------------------------------

(define call/cc call-with-current-continuation)

; (define current-lexical-environment
;   (environment ()
;     (list 'cdr (list 'lambda '() '()))))
;
; (define interaction-environment
;   (environment ()
;     (list 'cdr (list 'environment '() '()))))
;
; (define rename
;   (lambda (x)
;     (lambda () x)))

(define make-list
  (lambda (n . o)
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

(define newline
  (lambda ()
    (display "\n")))

(define swap!
  (environment (a b)
    (let ((x (symbol)))
     `(let ((,x ,a))
        (set! ,a ,b)
        (set! ,b ,x)))))

(define square
  (lambda (x)
    (* x x)))

(define close-file
  (lambda (object)
    (if (input-file? object)
        (close-input-file object)
        (if (output-file? object)
            (close-output-file object)
           '())))) ; TODO unspecified


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


