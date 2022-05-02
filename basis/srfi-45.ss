(define promise-tag (list 'promise))

(define (promise done? value-or-generator)
  (cons (cons done? value-or-generator) promise-tag))

(define (promise? x)
  (and (pair? x)
       (eq? promise-tag (cdr x))))

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

(define-syntax lazy
  (er-macro-transformer
    (lambda (form rename compare)
      `(,(rename 'promise) #f (,(rename 'lambda) () ,(cadr form))))))

(define-syntax delay
  (er-macro-transformer
    (lambda (form rename compare)
      `(,(rename 'lazy) (,(rename 'promise) #t ,(cadr form))))))

(define (make-promise x)
  (if (promise? x) x
      (delay x)))
