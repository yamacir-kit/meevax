(import (scheme base)
        (scheme process-context)
        (scheme write)
        (srfi 78)
        (only (meevax core) call-with-current-continuation!)
        (only (meevax experimental) disassemble)
        )

; ------------------------------------------------------------------------------

(check (values) => ((values)))
(check (values 1) => 1)
(check (values 1 2) => ((values) 1 2))

(define (print . xs)
  (for-each (lambda (x)
              (display x))
            xs)
  (newline))

(define-values a (values 1 2 3))
(check a => (1 2 3))

(define-values (b) (values 1))
(check b => 1)

(define-values (c d) (values 1 2))
(check c => 1)
(check d => 2)

(define-values (e f g) (values 1 2 3))
(check e => 1)
(check f => 2)
(check g => 3)

(define-values (h . i) (values 1 2 3))
(check h => 1)
(check i => (2 3))

; (let ((j 1)
;       (k 2)
;       (l 3))
;   (define-values (j k l) (values 4 5 6))
;   (check j => 4)
;   (check k => 5)
;   (check l => 6))

(exit)

; ------------------------------------------------------------------------------

(define (values . xs)
  (call-with-current-continuation!
    (lambda (cc)
      (cc . xs))))

(check (values 1 2) => 1)

(check (values 3 4) => 3)

(check (+ 1 (values 2 3)) => 3)

(check (+ (values 1 2)
          (values 3 4)) => 4)

(let ((x (values 1 2 3)))
  (check x => 1))

((lambda xs
   (check xs => (1 2 3))
  )
 (values 1 2 3)
 )

; (check (begin 1 (values 2 3 4) 5) => 5)

; (let ()
;   (display "#1\n")
;   (values 1 2 3)
;   (display "#2\n"))


