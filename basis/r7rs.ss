(define (list-set! x k object) (set-car! (list-tail x k) object))
