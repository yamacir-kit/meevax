(define (list-set! x k object) (set-car! (list-tail x k) object))

(define interaction-environment
  (let ((e (fork/csc identity)))
    (lambda () e)))
