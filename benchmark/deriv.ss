; https://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/scheme/code/bench/gabriel/0.html

(import (scheme base)
        (scheme write)
        (scheme process-context))

(define (deriv-aux a)
  (list '/ (deriv a) a))

(define (deriv a)
  (cond ((not (pair? a))
         (cond ((eq? a 'x) 1)
               (else 0)))
        ((eq? (car a) '+)
         (cons '+ (map deriv (cdr a))))
        ((eq? (car a) '-)
         (cons '- (map deriv (cdr a))))
        ((eq? (car a) '*)
         (list '*
               a
               (cons '+ (map deriv-aux (cdr a)))))
        ((eq? (car a) '/)
         (list '-
               (list '/
                     (deriv (cadr a))
                     (caddr a))
               (list '/
                     (cadr a)
                     (list '*
                           (caddr a)
                           (caddr a)
                           (deriv (caddr a))))))
        (else 'error)))

(write (deriv '(+ (* 3 x x) (* a x x) (* b x) 5)))
(newline)

(do ((i 0 (+ i 1)))
    ((= i 5000))
  (deriv '(+ (* 3 x x) (* a x x) (* b x) 5)))

(exit)
