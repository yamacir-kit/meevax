; https://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/scheme/code/bench/gabriel/0.html

(import (scheme base)
        (scheme write)
        (scheme process-context))

(define (ctak x y z)
  (call-with-current-continuation
    (lambda (k)
      (ctak-aux k x y z))))

(define (ctak-aux k x y z)
  (cond ((not (< y x))  ;xy
         (k z))
        (else (call-with-current-continuation
                (ctak-aux
                  k
                  (call-with-current-continuation
                    (lambda (k)
                      (ctak-aux k
                                (- x 1)
                                y
                                z)))
                  (call-with-current-continuation
                    (lambda (k)
                      (ctak-aux k
                                (- y 1)
                                z
                                x)))
                  (call-with-current-continuation
                    (lambda (k)
                      (ctak-aux k
                                (- z 1)
                                x
                                y))))))))

(write (ctak 18 12 6))
(newline)

(exit)
