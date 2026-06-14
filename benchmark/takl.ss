(import (scheme base)
        (scheme write)
        (scheme process-context))

(define (shorterp x y)
  (and (pair? y)
       (or (null? x)
           (shorterp (cdr x)
                     (cdr y)))))

(define (mas x y z)
  (if (not (shorterp y x))
      z
      (mas (mas (cdr x) y z)
           (mas (cdr y) z x)
           (mas (cdr z) x y))))

(display (length (mas (make-list 18)
                      (make-list 12)
                      (make-list  6))))
(newline)

(exit)
