; ---- 4.2.5. Delayed evaluation -----------------------------------------------

; (define delay-force lazy) ; from SRFI-45

(define (make-promise x)
  (if (promise? x) x
      (delay x)))

; ---- 6.4. Pairs and lists ----------------------------------------------------

(define (list-set! x k object) (set-car! (list-tail x k) object))

(define interaction-environment
  (let ((e (fork/csc identity)))
    (lambda () e)))
