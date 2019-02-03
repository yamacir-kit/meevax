(quote a)
'a
(quote (a b c))

; (atom 'a)
; (atom '(a b c))
; (atom ())
; (atom (atom 'a))
; (atom '(atom 'a))

(eq? 'a 'a)
(eq? 'a 'b)
(eq? '() '())

(car '(a b c))
(cdr '(a b c))

(cons 'a '(b c))
(cons 'a (cons 'b (cons 'c ())))

(car (cons 'a '(b c)))
(cdr (cons 'a '(b c)))

; (if (eq? 'a 'b)
;     'first
;     (if (atom? 'a) 'second 'third))

((lambda (x) (cons x '(b))) 'a)
((lambda (x y) (cons x (cdr y))) 'z '(a b c))
((lambda (f) (f '(b c))) (lambda (x) (cons 'a x)))

(define null? (lambda (x)
  (eq? x ())
))

(null? 'a)
(null? ())

(define not (lambda (x)
  (if x #false #true)
))

(not #true)
(not #false)

(define and (lambda (x y)
  (if x
      (if y #true #false)
      #false)
))

(and (not (pair? 'a)) (eq? 'a 'a))
(and (not (pair? 'a)) (eq? 'a 'b))

(not (eq? 'a 'a))
(not (eq? 'a 'b))

(define append (lambda (x y)
  (if (null? x)
      y
      (cons (car x)
            (append (cdr x) y)))
))

(append '(a b) '(c d))
(append () '(c d))

(define list (lambda (x y)
  (cons x (cons y ()))))

(define zip (lambda (x y)
  (if (and (null? x) (null? y))
    ()
  (if (and (pair? x) (pair? y))
    (cons (list (car x) (car y))
          (zip  (cdr x) (cdr y)))
    ()
  ))
))

(zip '(x y z) '(a b c)); => ((x a) (y b) (z c))

(define caar (lambda (x) (car (car x))))
(define cadr (lambda (x) (car (cdr x))))
(define cadar (lambda (x) (car (cdr (car x)))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define caddar (lambda (x) (car (cdr (cdr (car x))))))
(define cadddr (lambda (x) (car (cdr (cdr (cdr x))))))

(define assoc (lambda (x y)
  (if (null? x)
    ()
  (if (null? y)
    x
    (if (eq? (caar y) x)
      (cadar y)
      (assoc x (cdr y)))
  ))
))

; (define assoc (lambda (x y)
;   (cond ((null x) ())
;         ((null y) x)
;         (else (cond ((eq (caar y) x) (cadar y))
;                          (else (assoc x (cdr y))))))))

(assoc 'x '((x a) (y b))); => a
(assoc 'x '((x new) (x a) (y b))); => new

(define evcon (lambda (c a)
  (if (eval (caar c) a)
    (eval (cadar c) a)
    (evcon (cdr c) a))
))

(define evlis (lambda (m a)
  (if (null m)
    ()
    (cons (eval  (car m) a)
          (evlis (cdr m) a)))
))

(define eval (lambda (e a)
  (cond
    ((atom e)
     (assoc e a))
    ((atom (car e))
     (cond ((eq (car e) 'quote) (car (cdr e)))
           ((eq (car e) 'atom) (atom (eval (cadr e) a)))
           ((eq (car e) 'eq) (eq (eval (cadr e) a) (eval (caddr e) a)))
           ((eq (car e) 'car) (car (eval (cadr e) a)))
           ((eq (car e) 'cdr) (cdr (eval (cadr e) a)))
           ((eq (car e) 'cons) (cons (eval (cadr e) a) (eval (caddr e) a)))
           ((eq (car e) 'cond) (evcon (cdr e) a))
           (else (eval (cons (assoc (car e) a) (cdr e)) a))))
    ((eq (caar e) 'recursive)
     (eval (cons (caddar e) (cdr e)) (cons (list (cadar e) (car e)) a)))
    ((eq (caar e) 'lambda)
     (eval (caddar e) (append (zip (cadar e) (evlis (cdr e) a)) a)))
    (else 'error))))

(eval '(quote a) ())
(eval ''a ())
(eval '(quote (a b c)) ())

(eval '(atom 'a) ())
(eval '(atom (quote (a b c))) ())
(eval '(atom ()) ())
(eval '(atom (atom 'a)) ())
(eval '(atom (quote (atom 'a))) ())

(eval '(eq 'a 'a) ())
(eval '(eq 'a 'b) ())
(eval '(eq () ()) ())

(eval '(car '(a b c)) ())
(eval '(cdr '(a b c)) ())

(eval '(cons 'a '(b c)) ())
(eval '(cons 'a (cons 'b (cons 'c '()))) ())
(eval '(car (cons 'a '(b c))) ())
(eval '(cdr (cons 'a '(b c))) ())

(eval '(cond ((eq 'a 'b) 'first) ((atom 'a) 'second)) ())

(eval '((lambda (x) (cons x '(b))) 'a) ())
(eval '((lambda (x y) (cons x (cdr y))) 'z '(a b c)) ())
(eval '((lambda (f) (f '(b c))) '(lambda (x) (cons 'a x))) ())

(eval '((recursive substitute (lambda (x y z)
          (cond ((atom z)
                 (cond ((eq z y) x)
                       (else z)))
                (else
                 (cons (substitute x y (car z))
                       (substitute x y (cdr z)))))))
      'm 'b '(a b (a b c) d))
      ())
