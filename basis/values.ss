; (define values
;   (lambda xs
;     (call-with-current-continuation
;       (lambda (cc)
;         (apply cc xs)))))

(define <values>
  ((lambda x x) 'values))

(define (values? x)
  (and (pair? x)
       (eq? (car x) <values>))) ; Magic Token Trick

(define (values . xs)
  (if (and (not (null? xs))
           (null? (cdr xs)))
      (car xs)
      (cons <values> xs)))

(define (call-with-values producer consumer)
  (let ((result (producer)))
    (if (values? result)
        (apply consumer (cdr result))
        (consumer result))))

; TODO
; (define (call-with-values producer consumer)
;   (let-values ((xs (producer)))
;     (apply consumer xs)))
