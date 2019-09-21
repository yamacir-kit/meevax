; ------------------------------------------------------------------------------
;   Link Externals
; ------------------------------------------------------------------------------

(define experimental.so
  (linker "./lib/libmeevax-experimental.so"))

(define display        (link experimental.so "display"))
(define emergency-exit (link experimental.so "emergency_exit"))

; ------------------------------------------------------------------------------
;   Miscellaneous
; ------------------------------------------------------------------------------

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

