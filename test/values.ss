(import (scheme base)
        (scheme write)
        (srfi 78)
        (only (meevax experimental) disassemble)
        (only (meevax syntax) call-with-current-continuation!)
        )

; ------------------------------------------------------------------------------

(define <values> (list 'values))

(check <values> => (values))

(define (values . xs)
  (if (and (pair? xs)
           (null? (cdr xs)))
      (car xs)
      (cons <values> xs)))

(check (values) => ((values)))
(check (values 1) => 1)
(check (values 1 2) => ((values) 1 2))

(define (call-with-values produce consume)
  (define (values? x)
    (and (pair? x)
         (eq? <values> (car x))))
  (let ((vs (produce)))
    (if (values? vs)
        (apply consume (cdr vs))
        (consume vs))))

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


