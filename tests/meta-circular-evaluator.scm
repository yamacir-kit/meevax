(define else #true)

(quote a)
'a
(quote (a b c))

(atom 'a)
(atom '(a b c))
(atom nil)
(atom (atom 'a))
(atom '(atom 'a))

(eq 'a 'a)
(eq 'a 'b)
(eq '() '())

(car '(a b c))
(cdr '(a b c))

(cons 'a '(b c))
(cons 'a (cons 'b (cons 'c nil)))

(car (cons 'a '(b c)))
(cdr (cons 'a '(b c)))

(cond ((eq 'a 'b) 'first)
      ((atom 'a) 'second))

((lambda (x) (cons x '(b))) 'a)
((lambda (x y) (cons x (cdr y))) 'z '(a b c))
((lambda (f) (f '(b c))) '(lambda (x) (cons 'a x)))

(define null (lambda (x)
  (eq x nil)))

(null 'a)
(null nil)

(define and (lambda (x y)
  (cond (x
         (cond (y 'true)
               (else nil)))
        (else nil))))

(and (atom 'a) (eq 'a 'a))
(and (atom 'a) (eq 'a 'b))

(define not (lambda (x)
  (cond (x nil)
        (else 'true))))

(not (eq 'a 'a))
(not (eq 'a 'b))

(define append (lambda (x y)
  (cond ((null x) y)
        (else (cons (car x)
                    (append (cdr x) y))))))

(append '(a b) '(c d))
(append nil '(c d))

(define list (lambda (x y)
  (cons x (cons y nil))))

(define zip (lambda (x y)
  (cond ((and (null x)
              (null y))
         nil)
        ((and (not (atom x))
              (not (atom y)))
         (cons (list (car x) (car y))
               (zip (cdr x) (cdr y))))
        (else nil))))

(zip '(x y z) '(a b c))

(define caar (lambda (x) (car (car x))))
(define cadr (lambda (x) (car (cdr x))))
(define cadar (lambda (x) (car (cdr (car x)))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define caddar (lambda (x) (car (cdr (cdr (car x))))))
(define cadddr (lambda (x) (car (cdr (cdr (cdr x))))))

(define assoc (lambda (x y)
  (cond ((null x) nil)
        ((null y) x)
        (else (cond ((eq (caar y) x) (cadar y))
                         (else (assoc x (cdr y))))))))

(assoc 'x '((x a) (y b)))
(assoc 'x '((x new) (x a) (y b)))

(define evcon (lambda (c a)
  (if (eval (caar c) a)
    (eval (cadar c) a)
    (evcon (cdr c) a))))

(define evlis (lambda (m a)
  (if (null m)
      nil
      (cons (eval (car m) a)
            (evlis (cdr m) a)))))

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

(eval '(quote a) nil)
(eval ''a nil)
(eval '(quote (a b c)) nil)

(eval '(atom 'a) nil)
(eval '(atom (quote (a b c))) nil)
(eval '(atom nil) nil)
(eval '(atom (atom 'a)) nil)
(eval '(atom (quote (atom 'a))) nil)

(eval '(eq 'a 'a) nil)
(eval '(eq 'a 'b) nil)
(eval '(eq nil nil) nil)

(eval '(car '(a b c)) nil)
(eval '(cdr '(a b c)) nil)

(eval '(cons 'a '(b c)) nil)
(eval '(cons 'a (cons 'b (cons 'c '()))) nil)
(eval '(car (cons 'a '(b c))) nil)
(eval '(cdr (cons 'a '(b c))) nil)

(eval '(cond ((eq 'a 'b) 'first) ((atom 'a) 'second)) nil)

(eval '((lambda (x) (cons x '(b))) 'a) nil)
(eval '((lambda (x y) (cons x (cdr y))) 'z '(a b c)) nil)
(eval '((lambda (f) (f '(b c))) '(lambda (x) (cons 'a x))) nil)

(eval '((recursive substitute (lambda (x y z)
          (cond ((atom z)
                 (cond ((eq z y) x)
                       (else z)))
                (else
                 (cons (substitute x y (car z))
                       (substitute x y (cdr z)))))))
      'm 'b '(a b (a b c) d))
      nil)
