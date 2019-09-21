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

(define close-file
  (lambda (object)
    (if (input-file? object)
        (close-input-file object)
        (if (output-file? object)
            (close-output-file object)
           '())))) ; TODO unspecified

