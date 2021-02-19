; (define values
;   (lambda xs
;     (call-with-current-continuation
;       (lambda (cc)
;         (apply cc xs)))))

(define <values>
  ((lambda x x) 'values))

(define (values? x)
  (if (pair? x)
      (eq? (car x) <values>) ; Magic Token Trick
      #f))

(define (values . xs)
  (if (if (null? xs) #f
          (null? (cdr xs)))
      (car xs)
      (cons <values> xs)))

(define (call-with-values producer consumer)
  ((lambda (vs)
     (if (values? vs)
         (apply consumer (cdr vs))
         (consumer vs)))
   (producer)))

; TODO
; (define (call-with-values producer consumer)
;   (let-values ((xs (producer)))
;     (apply consumer xs)))
