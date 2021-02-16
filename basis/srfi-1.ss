; ---- SRFI 1: List Library ----------------------------------------------------
;
;  https://srfi.schemers.org/srfi-1/srfi-1.html
;
;  Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;  this code as long as you do not remove this copyright notice or
;  hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;      -Olin
;
;  Tweaks:
;    * Strip derived expression types into primitive expression types.
;
; ------------------------------------------------------------------------------

; BUILTIN cons

(define (list . xs) xs)

(define (xcons x y)
  (cons y x))

(define (cons* x . xs)
  (define (cons* x xs)
    (if (pair? xs)
        (cons x (cons* (car xs)
                       (cdr xs)))
        x))
  (cons* x xs))

; TODO make-list
; TODO list-tabulate

(define (list-ref x k)
  (car (drop x k)))

(define (list-copy x)
  (define (list-copy x)
    (if (pair? x)
        (cons (car x)
              (list-copy (cdr x)))
        x))
  (list-copy x))

(define (circular-list x . xs)
  ((lambda (result)
     (set-cdr! (last-pair result) result)
     result)
   (cons x xs)))

; TODO iota

; BUILTIN pair?

(define (null? x)
  (eqv? x '()))

(define (proper-list? x)
  (define (lp x lag)
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
        (null? x)))
  (lp x x))

; TODO circular-list?
; TODO dotted-list?

(define (not-pair? x)
  (not (pair? x)))

(define (null-list? x)
  (if (pair? x) #f
      (if (null? x) #t
          (error "null-list?: argument out of domain" x))))

; TODO list=

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (first   x) (car x))
(define (second  x) (car (cdr x)))
(define (third   x) (car (cdr (cdr x))))
(define (fourth  x) (car (cdr (cdr (cdr x)))))
(define (fifth   x) (car (cdr (cdr (cdr (cdr x))))))
(define (sixth   x) (car (cdr (cdr (cdr (cdr (cdr x)))))))
(define (seventh x) (car (cdr (cdr (cdr (cdr (cdr (cdr x))))))))
(define (eighth  x) (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr x)))))))))
(define (ninth   x) (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr x))))))))))
(define (tenth   x) (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr x)))))))))))

(define (car+cdr pair)
  (values (car pair)
          (cdr pair)))

(define (take x k)
  (define (take x k)
    (if (zero? k) '()
        (cons (car x)
              (take (cdr x)
                    (- k 1)))))
  (take x k))

(define (take! x k)
  (if (zero? k)
      (begin (set-cdr! (drop x (- k 1)) '()) x)))

(define (take-right x k)
  (define (take-right a b)
    (if (pair? b)
        (take-right (cdr a)
                    (cdr b))
        a))
  (take-right x (drop x k)))

(define (drop x k)
  (define (drop x k)
    (if (zero? k) x
        (drop (cdr x)
              (- k 1))))
  (drop x k))

(define (drop! x k)
  (if (negative? k)
      ((lambda (nelts)
         (if (zero? nelts) '()
             (begin (set-cdr! (list-tail x (- nelts 1)) '()) x)))
        (+ k (length x)))
      (list-tail x k)))

; TODO drop-right
; TODO drop-right!
; TODO split-at
; TODO split-at!
; TODO last
; TODO last-pair

(define (length x)
  (define (length x k)
    (if (pair? x)
        (length (cdr x)
                (+ k 1))
        k))
  (length x 0))

; length+

(define (append . xs)
  (define (append x xs)
    (if (pair? xs)
        ((lambda (xs)
           (fold-right cons xs x))
         (append (car xs)
                 (cdr xs)))
        x))
  (if (pair? xs)
      (append (car xs)
              (cdr xs))
      '()))

; append!

; TODO AFTER fold
; (define reverse
;   (lambda (x)
;     (fold cons '() x)))

(define (reverse x)
  (define (append-2 x y) ; from SICP
    (if (null? x) y
        (cons (car x)
              (append-2 (cdr x) y))))
  (if (null? x) '()
      (append-2 (reverse (cdr x))
                (list (car x)))))

(define reverse!
  (lambda (x)
    (define reverse!
      (lambda (x result)
        (if (null-list? x) result
            ((lambda (tail)
               (set-cdr! x result)
               (reverse! tail x))
             (cdr x)))))
    (reverse! x '())))

(define concatenate
  (lambda (xs)
    (reduce-right append '() xs)))

(define concatenate!
  (lambda (xs)
    (reduce-right append! '() xs)))

(define append-reverse
  (lambda (head tail)
    (fold cons tail head)))

(define append-reverse!
  (lambda (head tail)
    (pair-fold (lambda (pair tail)
                 (set-cdr! pair tail) pair)
               tail
               head)))

(define zip
  (lambda (x . xs)
    (apply map list x xs)))

(define unzip1
  (lambda (x)
    (map car x)))

; unzip2
; unzip3
; unzip4
; unzip5
; count

; map
; map!
; for-each

(define fold-right
  (lambda (kons knil x . xs)
    (define fold-right-n
      (lambda (xs)
        ((lambda (cdrs)
           (if (null? cdrs) knil
               (apply kons (%cars+ xs (fold-right-n cdrs)))))
         (%cdrs xs))))
    (define fold-right-1
      (lambda (x)
        (if (null-list? x) knil
            ((lambda (head)
               (kons head (fold-right-1 (cdr x))))
             (car x)))))
    (if (pair? xs)
        (fold-right-n (cons x xs))
        (fold-right-1 x))))

; fold
; fold-right
; unfold
; unfold-right
; pair-fold
; pair-fold-right
; reduce
; reduce-right
; append-map
; append-map!
; pair-for-each
; filter-map
; map-in-order

(define filter
  (lambda (pred x)
    (define filter
      (lambda (x)
        (if (not-pair? x) x
            ((lambda (head tail)
               (if (pred head)
                   ((lambda (new-tail)
                      (if (eq? tail new-tail) x
                          (cons head new-tail)))
                    (filter tail))
                   (filter tail)))
             (car x)
             (cdr x)))))
    (filter x)))

; filter!
; partition
; partition!

(define remove
  (lambda (pred x)
    (filter (lambda (y)
              (not (pred y)))
            x)))

(define remove!
  (lambda (pred x)
    (filter! (lambda (y)
               (not (pred y)))
             x)))

(define find
  (lambda (compare x)
    ((lambda (result)
       (if result (car result) #f))
     (find-tail compare x))))

(define find-tail
  (lambda (compare x)
    (define find-tail
      (lambda (x)
        (if (null-list? x) #f
            (if (compare (car x)) x
                (find-tail (cdr x))))))
    (find-tail x)))

(define member
  (lambda (key x . compare)
    ((lambda (compare)
       (find-tail (lambda (x[i])
                    (compare key x[i]))
                  x))
     (if (pair? compare) (car compare) equal?))))

(define memq
  (lambda (key x)
    (member key x eq?)))

(define memv
  (lambda (key x)
    (member key x eqv?)))

; any
; every
; list-index
; take-while
; take-while!
; drop-while
; span
; span!

(define break
  (lambda (break? x)
    (span (lambda (x)
            (not (break? x)))
          x)))

(define break!
  (lambda (break? x)
    (span! (lambda (x)
             (not (break? x)))
           x)))

(define delete
  (lambda (key x . compare)
    ((lambda (compare)
       (filter (lambda (x[i])
                 (not (compare key x[i])))
               x))
     (if (pair? compare) (car compare) equal?))))

(define delete
  (lambda (key x . compare)
    ((lambda (compare)
       (filter! (lambda (x[i])
                  (not (compare key x[i])))
                x))
     (if (pair? compare) (car compare) equal?))))

; delete-duplicates
; delete-duplicates!

(define assoc
  (lambda (key alist . compare)
    ((lambda (compare)
       (find (lambda (each)
               (compare key (car each)))
             alist))
     (if (pair? compare) (car compare) equal?))))

(define assq
  (lambda (key alist)
    (assoc key alist eq?)))

(define assv
  (lambda (key alist)
    (assoc key alist eqv?)))

(define alist-cons
  (lambda (key datum alist)
    (cons (cons key datum) alist)))

(define alist-copy
  (lambda (alist)
    (map (lambda (each)
           (cons (car each)
                 (cdr each)))
         alist)))

(define alist-dalete
  (lambda (key alist . compare)
    ((lambda (compare)
       (filter (lambda (each)
                 (not (compare key (car each))))
               alist))
     (if (pair? compare) (car compare) equal?))))

(define alist-dalete!
  (lambda (key alist . compare)
    ((lambda (compare)
       (filter! (lambda (each)
                  (not (compare key (car each))))
                alist))
     (if (pair? compare) (car compare) equal?))))

; lset<=
; lset=
; lset-adjoin
; lset-union
; lset-union!
; lset-intersection
; lset-intersection!
; lset-difference
; lset-difference!
; lset-xor
; lset-xor!
; lset-diff+intersection
; lset-diff+intersection!

; set-car! (NOTE: SRFI-17)
; set-cdr! (NOTE: SRFI-17)

(define %cdrs ; (map cdr x)
  (lambda (x)
    (call-with-current-continuation
      (lambda (abort)
        (define cdrs
          (lambda (x)
            (if (pair? x)
                ((lambda (each)
                  (if (null-list? each)
                      (abort '())
                      (cons (cdr each)
                            (iterate (cdr x)))))
                 (car x))
                '())))
        (cdrs x)))))

(define %cars+
  (lambda (x y) ; (append! (map car x) (list y))
    (define cars+
      (lambda (x)
        (if (pair? x)
            (cons (caar x)
                  (cars+ (cdr x)))
            (list y))))
    (cars+ x)))
