; ---- SRFI 1: List Library ----------------------------------------------------
;
;  https://srfi.schemers.org/srfi-1/srfi-1.html
;
;  Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;  this code as long as you do not remove this copyright notice or
;  hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;      -Olin
;
;  TWEAKS:
;    * Strip derived expression types into primitive expression types.
;
; ------------------------------------------------------------------------------

; cons

(define list
  (lambda x x))

(define xcons
  (lambda (x y)
    (cons y x)))

; cons*
; make-list
; list-tabulate

(define list-copy
  (lambda (x)
    (define list-copy
      (lambda (x)
        (if (pair? x)
            (cons (car x)
                  (list-copy (cdr x)))
            x)))
    (list-copy x)))

(define circular-list
  (lambda (x . xs)
    ((lambda (result)
       (set-cdr! (last-pair result) result)
       result)
     (cons x xs))))

; iota

; pair?

(define null?
  (lambda (x)
    (eqv? x '())))

(define proper-list?
  (lambda (x)
    (define lp
      (lambda (x lag)
        (if (pair? x)
            ((lambda (x)
               (if (pair? x)
                   ((lambda (x lag)
                      (if (eq? x lag) #f
                          (lp x lag)))
                     (cdr x)
                     (cdr lag))
                   (null? x)))
             (cdr x))
            (null? x))))
    (lp x x)))

; circular-list? dotted-list?

(define not-pair?
  (lambda (x)
    (not (pair? x))))

(define null-list?
  (lambda (x)
    (if (pair? x) #f
        (if (null? x) #t
            (error "null-list?: argument out of domain" x)))))

; list=

(define caar (lambda (x) (car (car x))))
(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define cddr (lambda (x) (cdr (cdr x))))

(define caaar (lambda (x) (car (car (car x)))))
(define caadr (lambda (x) (car (car (cdr x)))))
(define cadar (lambda (x) (car (cdr (car x)))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define cdaar (lambda (x) (cdr (car (car x)))))
(define cdadr (lambda (x) (cdr (car (cdr x)))))
(define cddar (lambda (x) (cdr (cdr (car x)))))
(define cdddr (lambda (x) (cdr (cdr (cdr x)))))

(define caaaar (lambda (x) (car (car (car (car x))))))
(define caaadr (lambda (x) (car (car (car (cdr x))))))
(define caadar (lambda (x) (car (car (cdr (car x))))))
(define caaddr (lambda (x) (car (car (cdr (cdr x))))))
(define cadaar (lambda (x) (car (cdr (car (car x))))))
(define cadadr (lambda (x) (car (cdr (car (cdr x))))))
(define caddar (lambda (x) (car (cdr (cdr (car x))))))
(define cadddr (lambda (x) (car (cdr (cdr (cdr x))))))
(define cdaaar (lambda (x) (cdr (car (car (car x))))))
(define cdaadr (lambda (x) (cdr (car (car (cdr x))))))
(define cdadar (lambda (x) (cdr (car (cdr (car x))))))
(define cdaddr (lambda (x) (cdr (car (cdr (cdr x))))))
(define cddaar (lambda (x) (cdr (cdr (car (car x))))))
(define cddadr (lambda (x) (cdr (cdr (car (cdr x))))))
(define cdddar (lambda (x) (cdr (cdr (cdr (car x))))))
(define cddddr (lambda (x) (cdr (cdr (cdr (cdr x))))))

(define first   (lambda (x) (car x)))
(define second  (lambda (x) (car (cdr x))))
(define third   (lambda (x) (car (cdr (cdr x)))))
(define fourth  (lambda (x) (car (cdr (cdr (cdr x))))))
(define fifth   (lambda (x) (car (cdr (cdr (cdr (cdr x)))))))
(define sixth   (lambda (x) (car (cdr (cdr (cdr (cdr (cdr x))))))))
(define seventh (lambda (x) (car (cdr (cdr (cdr (cdr (cdr (cdr x)))))))))
(define eighth  (lambda (x) (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr x))))))))))
(define ninth   (lambda (x) (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr x)))))))))))
(define tenth   (lambda (x) (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr x))))))))))))

(define list-ref
  (lambda (x k)
    (car (drop x k))))

; car+cdr

(define take
  (lambda (x k)
    (define take
      (lambda (x k)
        (if (zero? k) '()
            (cons (car x)
                  (take (cdr x)
                        (- k 1))))))
    (take x k)))

(define take!
  (lambda (x k)
    (if (zero? k)
        (begin (set-cdr! (drop x (- k 1)) '()) x))))

(define drop
  (lambda (x k)
    (define drop
      (lambda (x k)
        (if (zero? k) x
            (drop (cdr x)
                  (- k 1)))))
    (drop x k)))

; take-right drop-right
; drop-right!
; split-at   split-at!
; last last-pair

(define length
  (lambda (x)
    (define length
      (lambda (x k)
        (if (pair? x)
            (length (cdr x)
                    (+ k 1))
            k)))
    (length x 0)))

; length+

; append  reverse
; append! reverse!

(define concatenate
  (lambda (xs)
    (reduce-right append '() xs)))

(define concatenate!
  (lambda (xs)
    (reduce-right append! '() xs)))

; append-reverse append-reverse!

(define zip
  (lambda (x . xs)
    (apply map list x xs)))

(define unzip1
  (lambda (x)
    (map car x)))

; unzip2 unzip3 unzip4 unzip5
; count

; map for-each

; unfold       pair-fold       reduce

; unfold-right pair-fold-right reduce-right
; append-map append-map!
; map! pair-for-each filter-map map-in-order

; filter  partition  remove
; filter! partition! remove!

; member memq memv
; find find-tail
; any every
; list-index
; take-while drop-while take-while!
; span break span! break!

; delete  delete-duplicates
; delete! delete-duplicates!

; assoc assq assv
; alist-cons alist-copy
; alist-delete alist-delete!

; lset<= lset= lset-adjoin
; lset-union      lset-union!
; lset-intersection    lset-intersection!
; lset-difference            lset-difference!
; lset-xor      lset-xor!
; lset-diff+intersection          lset-diff+intersection!

; set-car!
; set-cdr!
