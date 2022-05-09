; ------------------------------------------------------------------------------
;
;  https://srfi.schemers.org/srfi-1/srfi-1.html
;
;  Copyright (c) 1998, 1999 by Olin Shivers.
;
;  You may do as you please with this code as long as you do not remove this
;  copyright notice or hold me liable for its use. Please send bug reports to
;  shivers@ai.mit.edu. -Olin
;
; ------------------------------------------------------------------------------

; cons

; list

(define (xcons x y)
  (cons y x))

(define (tree-copy x)
  (letrec ((tree-copy (lambda (x)
                        (if (not (pair? x)) x
                            (cons (tree-copy (car x))
                                  (tree-copy (cdr x)))))))
    (tree-copy x)))

(define (make-list len . maybe-elt)
  (let ((elt (cond ((null? maybe-elt) #f) ; Default value
                   ((null? (cdr maybe-elt)) (car maybe-elt))
                   (else (error "Too many arguments to MAKE-LIST"
                                (cons len maybe-elt))))))
    (do ((i len (- i 1))
         (ans '() (cons elt ans)))
      ((<= i 0) ans))))

(define (list-tabulate len proc)
  (do ((i (- len 1) (- i 1))
       (ans '() (cons (proc i) ans)))
    ((< i 0) ans)))

(define (cons* first . rest)
  (let rec ((x first)
            (rest rest))
    (if (pair? rest)
        (cons x (rec (car rest)
                     (cdr rest)))
        x)))

(define (list-copy x)
  (let rec ((x x))
    (if (pair? x)
        (cons (car x) (rec (cdr x)))
        x)))

; (define (iota count . maybe-start+step)
;   (if (< count 0) (error "Negative step count" iota count))
;   (let-optionals maybe-start+step ((start 0) (step 1))
;     (let loop ((n 0) (r '()))
;       (if (= n count)
;           (reverse r)
;           (loop (+ 1 n)
;                 (cons (+ start (* n step)) r))))))

(define (circular-list val1 . vals)
  (let ((ans (cons val1 vals)))
    (set-cdr! (last-pair ans) ans)
    ans))

(define (proper-list? x)
  (let rec ((x x) (lag x))
    (if (pair? x)
        (let ((x (cdr x)))
          (if (pair? x)
              (let ((x   (cdr x))
                    (lag (cdr lag)))
                (and (not (eq? x lag)) (rec x lag)))
              (null? x)))
        (null? x))))

(define list? proper-list?)

(define (dotted-list? x)
  (let rec ((x x) (lag x))
    (if (pair? x)
        (let ((x (cdr x)))
          (if (pair? x)
              (let ((x   (cdr x))
                    (lag (cdr lag)))
                (and (not (eq? x lag)) (rec x lag)))
              (not (null? x))))
        (not (null? x)))))

(define (circular-list? x)
  (let rec ((x x) (lag x))
    (and (pair? x)
         (let ((x (cdr x)))
           (and (pair? x)
                (let ((x   (cdr x))
                      (lag (cdr lag)))
                  (or (eq? x lag) (rec x lag))))))))

(define (not-pair? x) (not (pair? x)))

(define (null-list? x)
  (cond ((pair? x) #f)
        ((null? x) #t)
        (else (error "null-list?: argument out of domain" x))))

(define (list= = . lists)
  (or (null? lists) ; special case
      (let lp1 ((list-a (car lists))
                (others (cdr lists)))
        (or (null? others)
            (let ((list-b (car others))
                  (others (cdr others)))
              (if (eq? list-a list-b) ; EQ? => LIST=
                  (lp1 list-b others)
                  (let lp2 ((pair-a list-a)
                            (pair-b list-b))
                    (if (null-list? pair-a)
                        (and (null-list? pair-b)
                             (lp1 list-b others))
                        (and (not (null-list? pair-b))
                             (= (car pair-a)
                                (car pair-b))
                             (lp2 (cdr pair-a)
                                  (cdr pair-b)))))))))))

(define (length x)
  (let rec ((x x) (len 0))
    (if (pair? x)
        (rec (cdr x) (+ len 1))
        len)))

(define (length+ x) ; Returns #f if X is circular.
  (let rec ((x x) (lag x) (len 0))
    (if (pair? x)
        (let ((x (cdr x))
              (len (+ len 1)))
          (if (pair? x)
              (let ((x   (cdr x))
                    (lag (cdr lag))
                    (len (+ len 1)))
                (and (not (eq? x lag)) (rec x lag len)))
              len))
        len)))

(define (zip list1 . more-lists) (apply map list list1 more-lists))

; (define (caar x) (car (car x)))
; (define (cadr x) (car (cdr x)))
; (define (cdar x) (cdr (car x)))
; (define (cddr x) (cdr (cdr x)))

; (define (caaar x) (car (car (car x))))
; (define (caadr x) (car (car (cdr x))))
; (define (cadar x) (car (cdr (car x))))
; (define (caddr x) (car (cdr (cdr x))))
; (define (cdaar x) (cdr (car (car x))))
; (define (cdadr x) (cdr (car (cdr x))))
; (define (cddar x) (cdr (cdr (car x))))
; (define (cdddr x) (cdr (cdr (cdr x))))

; (define (caaaar x) (car (car (car (car x)))))
; (define (caaadr x) (car (car (car (cdr x)))))
; (define (caadar x) (car (car (cdr (car x)))))
; (define (caaddr x) (car (car (cdr (cdr x)))))
; (define (cadaar x) (car (cdr (car (car x)))))
; (define (cadadr x) (car (cdr (car (cdr x)))))
; (define (caddar x) (car (cdr (cdr (car x)))))
; (define (cadddr x) (car (cdr (cdr (cdr x)))))
; (define (cdaaar x) (cdr (car (car (car x)))))
; (define (cdaadr x) (cdr (car (car (cdr x)))))
; (define (cdadar x) (cdr (car (cdr (car x)))))
; (define (cdaddr x) (cdr (car (cdr (cdr x)))))
; (define (cddaar x) (cdr (cdr (car (car x)))))
; (define (cddadr x) (cdr (cdr (car (cdr x)))))
; (define (cdddar x) (cdr (cdr (cdr (car x)))))
; (define (cddddr x) (cdr (cdr (cdr (cdr x)))))

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
  (let rec ((x x)
            (k k))
    (if (zero? k) '()
        (cons (car x)
              (rec (cdr x) (- k 1))))))

(define (take! x k)
  (if (zero? k)
      (begin (set-cdr! (drop x (- k 1)) '()) x)))

(define (drop x k)
  (let rec ((x x) (k k))
    (if (zero? k) x
        (rec (cdr x) (- k 1)))))

(define (drop! lis k)
  (if (negative? k)
      (let ((nelts (+ k (length lis))))
        (if (zero? nelts) '()
            (begin (set-cdr! (list-tail lis (- nelts 1)) '())
                   lis)))
      (list-tail lis k)))

(define (take-right x k)
  (let lp ((lag x)
           (lead (drop x k)))
    (if (pair? lead)
        (lp (cdr lag)
            (cdr lead))
        lag)))

(define (drop-right x k)
  (let rec ((lag x) (lead (drop x k)))
    (if (pair? lead)
        (cons (car lag)
              (rec (cdr lag) (cdr lead)))
        '())))

(define (drop-right! x k)
  (let ((lead (drop x k)))
    (if (pair? lead)
        (let rec ((lag x)
                  (lead (cdr lead)))
          (if (pair? lead)
              (rec (cdr lag)
                   (cdr lead))
              (begin (set-cdr! lag '()) x)))
        '())))

(define (list-ref x k) (car (drop x k)))

(define (split-at x k)
  (let recur ((lis x) (k k))
    (if (zero? k) (values '() lis)
        (receive (prefix suffix) (recur (cdr lis) (- k 1))
          (values (cons (car lis) prefix) suffix)))))

(define (split-at! x k)
  (if (zero? k)
      (values '() x)
      (let* ((prev (drop x (- k 1)))
             (suffix (cdr prev)))
        (set-cdr! prev '())
        (values x suffix))))

(define (last x) (car (last-pair x)))

(define (last-pair lis)
  (let rec ((lis lis))
    (let ((tail (cdr lis)))
      (if (pair? tail) (rec tail) lis))))

(define (unzip1 lis) (map car lis))

(define (unzip2 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis) ; Use NOT-PAIR? to handle
        (let ((elt (car lis)))      ; dotted lists.
          (receive (a b) (recur (cdr lis))
            (values (cons (car  elt) a)
                    (cons (cadr elt) b)))))))

(define (unzip3 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis lis)
        (let ((elt (car lis)))
          (receive (a b c) (recur (cdr lis))
            (values (cons (car   elt) a)
                    (cons (cadr  elt) b)
                    (cons (caddr elt) c)))))))

(define (unzip4 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis lis lis)
        (let ((elt (car lis)))
          (receive (a b c d) (recur (cdr lis))
            (values (cons (car    elt) a)
                    (cons (cadr   elt) b)
                    (cons (caddr  elt) c)
                    (cons (cadddr elt) d)))))))

(define (unzip5 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis lis lis lis)
        (let ((elt (car lis)))
          (receive (a b c d e) (recur (cdr lis))
            (values (cons (car     elt) a)
                    (cons (cadr    elt) b)
                    (cons (caddr   elt) c)
                    (cons (cadddr  elt) d)
                    (cons (car (cddddr  elt)) e)))))))

; (define (append . xs)
;   (if (pair? xs)
;       (letrec ((append (lambda (x xs)
;                          (if (pair? xs)
;                              ((lambda (tail)
;                                 (fold-right cons tail x))
;                               (append (car xs)
;                                       (cdr xs)))
;                              x))))
;         (append (car xs)
;                 (cdr xs)))
;       '()))

(define (append! . lists)
  (let lp ((lists lists) (prev '())) ; First, scan through lists looking for a non-empty one.
    (if (not (pair? lists)) prev
        (let ((first (car lists))
              (rest (cdr lists)))
          (if (not (pair? first)) (lp rest first)
              (let lp2 ((tail-cons (last-pair first)) ; Now, do the splicing.
                        (rest rest))
                (if (pair? rest)
                    (let ((next (car rest))
                          (rest (cdr rest)))
                      (set-cdr! tail-cons next)
                      (lp2 (if (pair? next) (last-pair next) tail-cons)
                           rest))
                    first)))))))

; (define (append-reverse rev-head tail) (fold cons tail rev-head))
;
; (define (append-reverse! rev-head tail)
;   (pair-fold (lambda (pair tail)
;                (set-cdr! pair tail) pair)
;              tail
;              rev-head))

; Hand-inline the FOLD and PAIR-FOLD ops for speed.

(define (append-reverse rev-head tail)
  (let lp ((rev-head rev-head) (tail tail))
    (if (null-list? rev-head) tail
        (lp (cdr rev-head) (cons (car rev-head) tail)))))

(define (append-reverse! rev-head tail)
  (let lp ((rev-head rev-head) (tail tail))
    (if (null-list? rev-head) tail
        (let ((next-rev (cdr rev-head)))
          (set-cdr! rev-head tail)
          (lp next-rev rev-head)))))

(define (concatenate  xs) (reduce-right append  '() xs))
(define (concatenate! xs) (reduce-right append! '() xs))

; Return (map cdr lists).
; However, if any element of LISTS is empty, just abort and return '().
(define (%cdrs xs)
  (call-with-current-continuation!
    (lambda (abort)
      (letrec ((recur (lambda (xs)
                        (if (pair? xs)
                            ((lambda (x)
                               (if (null-list? x)
                                   (abort '())
                                   (cons (cdr x)
                                         (recur (cdr xs)))))
                             (car xs))
                            '()))))
        (recur xs)))))

(define (%cars+ lists last-elt) ; (append! (map car lists) (list last-elt))
  (letrec ((recur (lambda (lists)
                    (if (pair? lists)
                        (cons (caar lists)
                              (recur (cdr lists)))
                        (list last-elt)))))
    (recur lists)))

(define (%cars+cdrs lists)
  (call-with-current-continuation!
    (lambda (abort)
      (let recur ((lists lists))
        (if (pair? lists)
            (receive (list other-lists) (car+cdr lists)
              (if (null-list? list) (abort '() '()) ; LIST is empty -- bail out
                  (receive (a d) (car+cdr list)
                    (receive (cars cdrs) (recur other-lists)
                      (values (cons a cars) (cons d cdrs))))))
            (values '() '()))))))

(define (%cars+cdrs+ lists cars-final)
  (call-with-current-continuation!
    (lambda (abort)
      (let recur ((lists lists))
        (if (pair? lists)
            (receive (list other-lists) (car+cdr lists)
              (if (null-list? list) (abort '() '()) ; LIST is empty -- bail out
                  (receive (a d) (car+cdr list)
                    (receive (cars cdrs) (recur other-lists)
                      (values (cons a cars) (cons d cdrs))))))
            (values (list cars-final) '()))))))

(define (%cars+cdrs/no-test lists)
  (let recur ((lists lists))
    (if (pair? lists)
        (receive (list other-lists) (car+cdr lists)
          (receive (a d) (car+cdr list)
            (receive (cars cdrs) (recur other-lists)
              (values (cons a cars) (cons d cdrs)))))
        (values '() '()))))

(define (count pred list1 . lists)
  (if (pair? lists)
      (let lp ((list1 list1) (lists lists) (i 0))
        (if (null-list? list1) i
            (receive (as ds) (%cars+cdrs lists)
              (if (null? as) i
                  (lp (cdr list1) ds
                      (if (apply pred (car list1) as) (+ i 1) i))))))
      (let lp ((lis list1) (i 0))
        (if (null-list? lis) i
            (lp (cdr lis) (if (pred (car lis)) (+ i 1) i))))))

(define (unfold-right p f g seed . maybe-tail)
  (let lp ((seed seed)
           (ans (if (pair? maybe-tail) (car maybe-tail) '())))
    (if (p seed) ans
        (lp (g seed)
            (cons (f seed) ans)))))

(define (unfold p f g seed . maybe-tail-gen)
  (if (pair? maybe-tail-gen)
      (let ((tail-gen (car maybe-tail-gen)))
        (if (pair? (cdr maybe-tail-gen))
            (apply error "Too many arguments" unfold p f g seed maybe-tail-gen)
            (let recur ((seed seed))
              (if (p seed) (tail-gen seed)
                  (cons (f seed) (recur (g seed)))))))
      (let recur ((seed seed))
        (if (p seed) '()
            (cons (f seed) (recur (g seed)))))))

(define (fold kons knil lis1 . lists)
  (if (pair? lists)
      (let lp ((lists (cons lis1 lists)) (ans knil))
        (receive (cars+ans cdrs) (%cars+cdrs+ lists ans)
          (if (null? cars+ans) ans ; Done.
              (lp cdrs (apply kons cars+ans)))))
      (let lp ((lis lis1) (ans knil))
        (if (null-list? lis) ans
            (lp (cdr lis) (kons (car lis) ans))))))

(define (fold-right f knil x . xs)
  (if (pair? xs)
      (letrec ((recur (lambda (lists)
                        ((lambda (cdrs)
                           (if (null? cdrs) knil
                               (apply f (%cars+ lists (recur cdrs)))))
                         (%cdrs lists)))))
        (recur (cons x xs)))
      (letrec ((recur (lambda (x)
                        (if (null-list? x) knil
                            ((lambda (head)
                               (f head (recur (cdr x))))
                             (car x))))))
        (recur x))))

(define (pair-fold-right f zero lis1 . lists)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))
        (let ((cdrs (%cdrs lists)))
          (if (null? cdrs) zero
              (apply f (append! lists (list (recur cdrs)))))))
      (let recur ((lis lis1))
        (if (null-list? lis) zero (f lis (recur (cdr lis)))))))

(define (pair-fold f zero lis1 . lists)
  (if (pair? lists)
      (let lp ((lists (cons lis1 lists)) (ans zero))
        (let ((tails (%cdrs lists)))
          (if (null? tails) ans
              (lp tails (apply f (append! lists (list ans)))))))

      (let lp ((lis lis1) (ans zero))
        (if (null-list? lis) ans
            (let ((tail (cdr lis)))
              (lp tail (f lis ans)))))))

(define (reduce f ridentity lis)
  (if (null-list? lis) ridentity
      (fold f (car lis) (cdr lis))))

(define (reduce-right f ridentity lis)
  (if (null-list? lis) ridentity
      (let recur ((head (car lis)) (lis (cdr lis)))
        (if (pair? lis)
            (f head (recur (car lis) (cdr lis)))
            head))))

(define (append-map  f lis1 . lists) (really-append-map append-map  append  f lis1 lists))
(define (append-map! f lis1 . lists) (really-append-map append-map! append! f lis1 lists))

(define (really-append-map who appender f lis1 lists)
  (if (pair? lists)
      (receive (cars cdrs) (%cars+cdrs (cons lis1 lists))
        (if (null? cars) '()
            (let recur ((cars cars) (cdrs cdrs))
              (let ((vals (apply f cars)))
                (receive (cars2 cdrs2) (%cars+cdrs cdrs)
                  (if (null? cars2) vals
                      (appender vals (recur cars2 cdrs2))))))))
      (if (null-list? lis1) '()
          (let recur ((elt (car lis1)) (rest (cdr lis1)))
            (let ((vals (f elt)))
              (if (null-list? rest) vals
                  (appender vals (recur (car rest) (cdr rest)))))))))

(define (for-each f x . xs)
  (define (for-each f x)
    (if (pair? x)
        (begin (f (car x))
               (for-each f (cdr x)))))
  (if (null? xs)
      (for-each f x)
      (begin (apply map f x xs)
             (if #f #f))))

(define (pair-for-each proc lis1 . lists)
  (if (pair? lists)
      (let lp ((lists (cons lis1 lists)))
        (let ((tails (%cdrs lists)))
          (if (pair? tails)
              (begin (apply proc lists)
                     (lp tails)))))
      (let lp ((lis lis1))
        (if (not (null-list? lis))
            (let ((tail (cdr lis)))
              (proc lis)
              (lp tail))))))

(define (map! f lis1 . lists)
  (if (pair? lists)
      (let lp ((lis1 lis1) (lists lists))
        (if (not (null-list? lis1))
            (receive (heads tails) (%cars+cdrs/no-test lists)
              (set-car! lis1 (apply f (car lis1) heads))
              (lp (cdr lis1) tails))))
      (pair-for-each (lambda (pair) (set-car! pair (f (car pair)))) lis1))
  lis1)

(define (filter-map f lis1 . lists)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))
        (receive (cars cdrs) (%cars+cdrs lists)
          (if (pair? cars)
              (cond ((apply f cars) => (lambda (x) (cons x (recur cdrs))))
                    (else (recur cdrs))) ; Tail call in this arm.
              '())))
      (let recur ((lis lis1))
        (if (null-list? lis) lis
            (let ((tail (recur (cdr lis))))
              (cond ((f (car lis)) => (lambda (x) (cons x tail)))
                    (else tail)))))))

(define (map-in-order f lis1 . lists)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))
        (receive (cars cdrs) (%cars+cdrs lists)
          (if (pair? cars)
              (let ((x (apply f cars)))
                (cons x (recur cdrs)))
              '())))
      (let recur ((lis lis1))
        (if (null-list? lis) lis
            (let ((tail (cdr lis))
                  (x (f (car lis))))
              (cons x (recur tail)))))))

(define map map-in-order)

(define (filter pred lis)
  (let recur ((lis lis))
    (if (null-list? lis) lis
        (let ((head (car lis))
              (tail (cdr lis)))
          (if (pred head)
              (let ((new-tail (recur tail)))
                (if (eq? tail new-tail) lis
                    (cons head new-tail)))
              (recur tail))))))

; (define (filter pred lis) ; Another version that shares longest tail.
;   (receive (ans no-del?)
;     (let recur ((l l))
;       (if (null-list? l) (values l #t)
;           (let ((x  (car l))
;                 (tl (cdr l)))
;             (if (pred x)
;                 (receive (ans no-del?) (recur tl)
;                   (if no-del?
;                       (values l #t)
;                       (values (cons x ans) #f)))
;                 (receive (ans no-del?) (recur tl) ; Delete X.
;                   (values ans #f))))))
;     ans))

; Things are much simpler if you are willing to push N stack frames & do N
; set-cdr! writes, where N is the length of the answer.
(define (filter! pred lis)
  (let recur ((lis lis))
    (if (pair? lis)
        (cond ((pred (car lis))
               (set-cdr! lis (recur (cdr lis)))
               lis)
              (else (recur (cdr lis))))
        lis)))

; (define (filter! pred lis)
;   (let lp ((ans lis))
;     (cond ((null-list? ans) ans)
;           ((not (pred (car ans))) (lp (cdr ans)))
;           (else (letrec ((scan-in (lambda (prev lis)
;                                     (if (pair? lis)
;                                         (if (pred (car lis))
;                                             (scan-in lis (cdr lis))
;                                             (scan-out prev (cdr lis))))))
;                          (scan-out (lambda (prev lis)
;                                      (let lp ((lis lis))
;                                        (if (pair? lis)
;                                            (if (pred (car lis))
;                                                (begin (set-cdr! prev lis)
;                                                       (scan-in lis (cdr lis)))
;                                                (lp (cdr lis)))
;                                            (set-cdr! prev lis))))))
;                   (scan-in ans (cdr ans))
;                   ans)))))

(define (partition pred lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis) ; Use NOT-PAIR? to handle dotted lists.
        (let ((elt (car lis))
              (tail (cdr lis)))
          (receive (in out) (recur tail)
            (if (pred elt)
                (values (if (pair? out) (cons elt in) lis) out)
                (values in (if (pair? in) (cons elt out) lis))))))))

(define (partition! pred lis)
  (if (null-list? lis)
      (values lis lis)
      (letrec ((scan-in (lambda (in-prev out-prev lis)
                          (let lp ((in-prev in-prev) (lis lis))
                            (if (pair? lis)
                                (if (pred (car lis))
                                    (lp lis (cdr lis))
                                    (begin (set-cdr! out-prev lis)
                                           (scan-out in-prev lis (cdr lis))))
                                (set-cdr! out-prev lis)))))
               (scan-out (lambda (in-prev out-prev lis)
                           (let lp ((out-prev out-prev) (lis lis))
                             (if (pair? lis)
                                 (if (pred (car lis))
                                     (begin (set-cdr! in-prev lis)
                                            (scan-in lis out-prev (cdr lis)))
                                     (lp lis (cdr lis)))
                                 (set-cdr! in-prev lis))))))
        (if (pred (car lis))
            (let lp ((prev-l lis) (l (cdr lis)))
              (cond ((not (pair? l)) (values lis l))
                    ((pred (car l)) (lp l (cdr l)))
                    (else (scan-out prev-l l (cdr l))
                          (values lis l))))
            (let lp ((prev-l lis) (l (cdr lis)))
              (cond ((not (pair? l)) (values l lis))
                    ((pred (car l))
                     (scan-in l prev-l (cdr l))
                     (values l lis))
                    (else (lp l (cdr l)))))))))

(define (remove  satisfy? x) (filter  (lambda (y) (not (satisfy? y))) x))
(define (remove! satisfy? x) (filter! (lambda (y) (not (satisfy? y))) x))

(define (delete x lis . maybe-=)
  (let ((= (if (pair? maybe-=) (car maybe-=) equal?)))
    (filter (lambda (y) (not (= x y))) lis)))

(define (delete! x lis . maybe-=)
  (let ((= (if (pair? maybe-=) (car maybe-=) equal?)))
    (filter! (lambda (y) (not (= x y))) lis)))

(define (member x lis . maybe-=)
  (let ((= (if (pair? maybe-=) (car maybe-=) equal?)))
    (find-tail (lambda (y) (= x y)) lis)))

; (define (memq key x) (member key x eq?))
; (define (memv key x) (member key x eqv?))

(define (delete-duplicates lis . maybe-=)
  (let ((elt= (if (pair? maybe-=) (car maybe-=) equal?)))
    (let recur ((lis lis))
      (if (null-list? lis) lis
          (let* ((x (car lis))
                 (tail (cdr lis))
                 (new-tail (recur (delete x tail elt=))))
            (if (eq? tail new-tail) lis (cons x new-tail)))))))

(define (delete-duplicates! lis maybe-=)
  (let ((elt= (if (pair? maybe-=) (car maybe-=) equal?)))
    (let recur ((lis lis))
      (if (null-list? lis) lis
          (let* ((x (car lis))
                 (tail (cdr lis))
                 (new-tail (recur (delete! x tail elt=))))
            (if (eq? tail new-tail) lis (cons x new-tail)))))))

(define (assoc x lis . maybe-=)
  (let ((= (if (pair? maybe-=) (car maybe-=) equal?)))
    (find (lambda (entry) (= x (car entry))) lis)))

(define (assq key alist) (assoc key alist eq?))
(define (assv key alist) (assoc key alist eqv?))

(define (alist-cons key datum alist)
  (cons (cons key datum) alist))

(define (alist-copy alist)
  (map (lambda (each)
         (cons (car each)
               (cdr each)))
       alist))

(define (alist-delete key alist . maybe-=)
  (let ((= (if (pair? maybe-=) (car maybe-=) equal?)))
    (filter (lambda (elt) (not (= key (car elt)))) alist)))

(define (alist-delete! key alist . maybe-=)
  (let ((= (if (pair? maybe-=) (car maybe-=) equal?)))
    (filter! (lambda (elt) (not (= key (car elt)))) alist)))

(define (find pred list)
  (cond ((find-tail pred list) => car)
  (else #f)))

(define (find-tail pred list)
  (let lp ((list list))
    (and (not (null-list? list))
         (if (pred (car list)) list
             (lp (cdr list))))))

(define (take-while pred lis)
  (let recur ((lis lis))
    (if (null-list? lis) '()
        (let ((x (car lis)))
          (if (pred x)
              (cons x (recur (cdr lis)))
              '())))))

(define (take-while! pred lis)
  (if (or (null-list? lis) (not (pred (car lis)))) '()
      (begin (let lp ((prev lis) (rest (cdr lis)))
               (if (pair? rest)
                   (let ((x (car rest)))
                     (if (pred x) (lp rest (cdr rest))
                         (set-cdr! prev '())))))
             lis)))

(define (drop-while pred lis)
  (let lp ((lis lis))
    (if (null-list? lis) '()
        (if (pred (car lis))
            (lp (cdr lis))
            lis))))

(define (span pred lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values '() '())
        (let ((x (car lis)))
          (if (pred x)
              (receive (prefix suffix) (recur (cdr lis))
                (values (cons x prefix) suffix))
              (values '() lis))))))

(define (span! pred lis)
  (if (or (null-list? lis) (not (pred (car lis)))) (values '() lis)
      (let ((suffix (let lp ((prev lis) (rest (cdr lis)))
                      (if (null-list? rest) rest
                          (let ((x (car rest)))
                            (if (pred x) (lp rest (cdr rest))
                                (begin (set-cdr! prev '())
                                       rest)))))))
        (values lis suffix))))

(define (break  break? x) (span  (lambda (x) (not (break? x))) x))
(define (break! break? x) (span! (lambda (x) (not (break? x))) x))

(define (any pred lis1 . lists)
  (if (pair? lists)
      (receive (heads tails) (%cars+cdrs (cons lis1 lists))
        (and (pair? heads)
             (let lp ((heads heads) (tails tails))
               (receive (next-heads next-tails) (%cars+cdrs tails)
                 (if (pair? next-heads)
                     (or (apply pred heads) (lp next-heads next-tails))
                     (apply pred heads))))))
      (and (not (null-list? lis1))
           (let lp ((head (car lis1)) (tail (cdr lis1)))
             (if (null-list? tail)
                 (pred head)
                 (or (pred head) (lp (car tail) (cdr tail))))))))

(define (every pred lis1 . lists)
  (if (pair? lists)
      (receive (heads tails) (%cars+cdrs (cons lis1 lists))
        (or (not (pair? heads))
            (let lp ((heads heads) (tails tails))
              (receive (next-heads next-tails) (%cars+cdrs tails)
                (if (pair? next-heads)
                    (and (apply pred heads) (lp next-heads next-tails))
                    (apply pred heads))))))
      (or (null-list? lis1)
          (let lp ((head (car lis1))  (tail (cdr lis1)))
            (if (null-list? tail)
                (pred head)
                (and (pred head) (lp (car tail) (cdr tail))))))))

(define (list-index pred lis1 . lists)
  (if (pair? lists)
      (let lp ((lists (cons lis1 lists)) (n 0))
        (receive (heads tails) (%cars+cdrs lists)
          (and (pair? heads)
               (if (apply pred heads) n
                   (lp tails (+ n 1))))))
      (let lp ((lis lis1) (n 0))
        (and (not (null-list? lis))
             (if (pred (car lis)) n (lp (cdr lis) (+ n 1)))))))

(define (reverse xs)
  (fold cons '() xs))

(define (reverse! lis)
  (let lp ((lis lis) (ans '()))
    (if (null-list? lis) ans
        (let ((tail (cdr lis)))
          (set-cdr! lis ans)
          (lp tail lis)))))

(define (%lset2<= = lis1 lis2) (every (lambda (x) (member x lis2 =)) lis1))

(define (lset<= = . lists)
  (or (not (pair? lists)) ; 0-ary case
      (let lp ((s1 (car lists)) (rest (cdr lists)))
        (or (not (pair? rest))
            (let ((s2 (car rest))  (rest (cdr rest)))
              (and (or (eq? s2 s1) ; Fast path
                       (%lset2<= = s1 s2)) ; Real test
                   (lp s2 rest)))))))

(define (lset= = . lists)
  (or (not (pair? lists)) ; 0-ary case
      (let lp ((s1 (car lists)) (rest (cdr lists)))
        (or (not (pair? rest))
            (let ((s2   (car rest))
                  (rest (cdr rest)))
              (and (or (eq? s1 s2) ; Fast path
                       (and (%lset2<= = s1 s2) (%lset2<= = s2 s1))) ; Real test
                   (lp s2 rest)))))))

(define (lset-adjoin = lis . elts)
  (fold (lambda (elt ans) (if (member elt ans =) ans (cons elt ans)))
        lis elts))

(define (lset-union = . lists)
  (reduce (lambda (lis ans) ; Compute ANS + LIS.
            (cond ((null? lis) ans) ; Don't copy any lists
                  ((null? ans) lis) ; if we don't have to.
                  ((eq? lis ans) ans)
                  (else
                    (fold (lambda (elt ans) (if (any (lambda (x) (= x elt)) ans)
                                                ans
                                                (cons elt ans)))
                          ans lis))))
          '() lists))

(define (lset-union! = . lists)
  (reduce (lambda (lis ans) ; Splice new elts of LIS onto the front of ANS.
            (cond ((null? lis) ans) ; Don't copy any lists
                  ((null? ans) lis) ; if we don't have to.
                  ((eq? lis ans) ans)
                  (else
                    (pair-fold (lambda (pair ans)
                                 (let ((elt (car pair)))
                                   (if (any (lambda (x) (= x elt)) ans)
                                       ans
                                       (begin (set-cdr! pair ans) pair))))
                               ans lis))))
          '() lists))

(define (lset-intersection = lis1 . lists)
  (let ((lists (delete lis1 lists eq?))) ; Throw out any LIS1 vals.
    (cond ((any null-list? lists) '()) ; Short cut
          ((null? lists) lis1) ; Short cut
          (else (filter (lambda (x)
                          (every (lambda (lis) (member x lis =)) lists))
                        lis1)))))

(define (lset-intersection! = lis1 . lists)
  (let ((lists (delete lis1 lists eq?))) ; Throw out any LIS1 vals.
    (cond ((any null-list? lists) '()) ; Short cut
          ((null? lists)          lis1) ; Short cut
          (else (filter! (lambda (x)
                           (every (lambda (lis) (member x lis =)) lists))
                         lis1)))))

(define (lset-difference = lis1 . lists)
  (let ((lists (filter pair? lists))) ; Throw out empty lists.
    (cond ((null? lists)     lis1) ; Short cut
          ((memq lis1 lists) '()) ; Short cut
          (else (filter (lambda (x)
                          (every (lambda (lis) (not (member x lis =)))
                                 lists))
                        lis1)))))

(define (lset-difference! = lis1 . lists)
  (let ((lists (filter pair? lists))) ; Throw out empty lists.
    (cond ((null? lists)     lis1) ; Short cut
          ((memq lis1 lists) '()) ; Short cut
          (else (filter! (lambda (x)
                           (every (lambda (lis) (not (member x lis =)))
                                  lists))
                         lis1)))))

(define (lset-xor = . lists)
  (reduce (lambda (b a) ; Compute A xor B:
            ;; Note that this code relies on the constant-time
            ;; short-cuts provided by LSET-DIFF+INTERSECTION,
            ;; LSET-DIFFERENCE & APPEND to provide constant-time short
            ;; cuts for the cases A = (), B = (), and A eq? B. It takes
            ;; a careful case analysis to see it, but it's carefully
            ;; built in.

            ;; Compute a-b and a^b, then compute b-(a^b) and
            ;; cons it onto the front of a-b.
            (receive (a-b a-int-b) (lset-diff+intersection = a b)
              (cond ((null? a-b) (lset-difference = b a))
                    ((null? a-int-b) (append b a))
                    (else (fold (lambda (xb ans)
                                  (if (member xb a-int-b =) ans (cons xb ans)))
                                a-b
                                b)))))
          '() lists))

(define (lset-xor! = . lists)
  (reduce (lambda (b a) ; Compute A xor B:
            ;; Note that this code relies on the constant-time
            ;; short-cuts provided by LSET-DIFF+INTERSECTION,
            ;; LSET-DIFFERENCE & APPEND to provide constant-time short
            ;; cuts for the cases A = (), B = (), and A eq? B. It takes
            ;; a careful case analysis to see it, but it's carefully
            ;; built in.

            ;; Compute a-b and a^b, then compute b-(a^b) and
            ;; cons it onto the front of a-b.
            (receive (a-b a-int-b) (lset-diff+intersection! = a b)
              (cond ((null? a-b) (lset-difference! = b a))
                    ((null? a-int-b) (append! b a))
                    (else (pair-fold (lambda (b-pair ans)
                                       (if (member (car b-pair) a-int-b =) ans
                                           (begin (set-cdr! b-pair ans) b-pair)))
                                     a-b
                                     b)))))
          '() lists))

(define (lset-diff+intersection = lis1 . lists)
  (cond ((every null-list? lists) (values lis1 '())) ; Short cut
        ((memq lis1 lists) (values '() lis1)) ; Short cut
        (else (partition (lambda (elt)
                           (not (any (lambda (lis) (member elt lis =))
                                     lists)))
                         lis1))))

(define (lset-diff+intersection! = lis1 . lists)
  (cond ((every null-list? lists) (values lis1 '())) ; Short cut
        ((memq lis1 lists) (values '() lis1)) ; Short cut
        (else (partition! (lambda (elt)
                            (not (any (lambda (lis) (member elt lis =))
                                      lists)))
                          lis1))))
