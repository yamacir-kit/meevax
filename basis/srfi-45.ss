(define <promise> (list 'promise))

(define (promise done? closure)
  (cons (cons done? closure) <promise>))

(define (promise? x)
  (and (pair? x)
       (eq? <promise> (cdr x))))

(define promise-done? caar)
(define promise-value cdar)
(define promise-generator cdar)

(define (promise-merge! new old)
  (set-car! (car old) (promise-done? new))
  (set-cdr! (car old) (promise-value new))
  (set-car! new (car old)))

(define (force promise)
  (if (promise-done? promise)
      (promise-value promise)
      (let ((new ((promise-generator promise))))
        (unless (promise-done? promise)
                (promise-merge! new promise))
        (force promise))))

(define-syntax (lazy expression)
  (list promise #f (list lambda '() expression)))

(define-syntax (delay expression)
  (list lazy (list promise #t expression)))

(define (make-promise x)
  (if (promise? x) x
      (delay x)))
