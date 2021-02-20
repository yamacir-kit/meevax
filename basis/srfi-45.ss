(define <promise> (list 'promise))

 ; ((#f . #,(closure ...)) promise)
(define promise
  (lambda (forced? closure)
    (cons (cons forced? closure) <promise>)))

(define promise?
  (lambda (x)
    (and (pair? x)
         (eq? <promise> (cdr x)))))

(define force
  (lambda (promise)

    (define done? caar)

    (define cache cdar)

    (define update!
      (lambda (new old)
        (set-car! (car old) (done? new))
        (set-cdr! (car old) (cache new))
        (set-car! new (car old))))

    (if (done? promise)
        (cache promise)
        (let ((new ((cache promise))))
          (unless (done? promise)
                  (update! new promise))
          (force promise)))))

(define-syntax (delay-force expression)
  (list promise #f (list lambda '() expression)))

(define-syntax (delay expression)
  (list delay-force (list promise #t expression)))

; (define make-promise
;   (lambda (x)
;     (if (promise? x) x
;         (delay x))))
