#|
   Copyright (c) 1998, 1999 by Olin Shivers.

   You may do as you please with this code as long as you do not remove this
   copyright notice or hold me liable for its use. Please send bug reports to
   shivers@ai.mit.edu. -Olin
|#

(define-library (srfi 1)
  (import (only (meevax boolean) not)
          (only (meevax core) begin call-with-current-continuation! define if lambda letrec quote set!)
          (only (meevax list)
            alist-cons alist-copy append append! append-reverse append-reverse!
            assq assv circular-list circular-list? concatenate concatenate!
            dotted-list? drop drop-right drop-right! eighth fifth first fourth
            iota last last-pair length length+ list list? list-copy list-ref
            make-list memq memv ninth null? null-list? reverse reverse! second
            seventh sixth take take! take-right tenth third)
          (only (meevax pair)
            car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr
            cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
            cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr cons cons*
            not-pair? pair? set-car! set-cdr! xcons)
          (only (scheme r5rs)
            cond and or let let* eqv? eq? equal? = < zero? + - member assoc
            apply map for-each values)
          (only (srfi 8) receive)
          (only (srfi 23) error))

  (export ; Constructors
          cons list xcons cons* make-list list-tabulate list-copy circular-list
          iota

          ; Predicates
          pair? null? proper-list? circular-list? dotted-list? not-pair?
          null-list? list=

          ; Selectors
          car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar
          cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar
          cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
          list-ref
          first second third fourth fifth sixth seventh eighth ninth tenth
          car+cdr
          take take! take-right
          drop drop-right drop-right!
          split-at split-at!
          last last-pair

          ; Miscellaneous: length, append, concatenate, reverse, zip & count
          length length+
          append append!
          concatenate concatenate!
          reverse reverse!
          append-reverse append-reverse!
          zip unzip1 unzip2 unzip3 unzip4 unzip5
          count

          ; Fold, unfold & map
          map map! filter-map map-in-order
          fold fold-right
          unfold unfold-right
          pair-fold pair-fold-right
          reduce reduce-right
          append-map append-map!
          for-each pair-for-each

          ; Filtering & partitioning
          filter filter!
          partition partition!
          remove remove!

          ; Searching
          memq memv member
          find find-tail
          any every list-index
          take-while take-while!
          drop-while
          span span!
          break break!

          ; Deleting
          delete delete!
          delete-duplicates delete-duplicates!

          ; Association lists
          assq assv assoc alist-cons alist-copy alist-delete alist-delete!

          ; Set operations on lists
          lset<= lset=
          lset-adjoin
          lset-union lset-union!
          lset-intersection lset-intersection!
          lset-difference lset-difference!
          lset-xor lset-xor!
          lset-diff+intersection lset-diff+intersection!

          ; Primitive side-effects
          set-car! set-cdr!)

  (begin (define (list-tabulate k f)
           (let recur ((i 0))
             (if (< i k)
                 (cons (f i)
                       (recur (+ i 1)))
                 '())))

         (define proper-list? list?)

         (define (list= x=? . xss)
           (or (null? xss)
               (let outer ((xs (car xss))
                           (xss (cdr xss)))
                 (or (null? xss)
                     (let ((ys (car xss))
                           (xss (cdr xss)))
                       (if (eq? xs ys)
                           (outer ys xss)
                           (let inner ((a xs)
                                       (b ys))
                             (if (null-list? a)
                                 (and (null-list? b)
                                      (outer ys xss))
                                 (and (not (null-list? b))
                                      (x=? (car a)
                                           (car b))
                                      (inner (cdr a)
                                             (cdr b)))))))))))

         (define (car+cdr pair)
           (values (car pair)
                   (cdr pair)))

         (define (split-at xs k)
           (if (zero? k)
               (values '() xs)
               (receive (a b) (split-at (cdr xs)
                                        (- k 1))
                 (values (cons (car xs)
                               a)
                         b))))

         (define (split-at! x k)
           (if (zero? k)
               (values '() x)
               (let* ((prefix-last (drop x (- k 1)))
                      (suffix (cdr prefix-last)))
                 (set-cdr! prefix-last '())
                 (values x suffix))))

         (define (zip x . xs)
           (apply map list x xs))

         (define (unzip1 xs)
           (map car xs))

         (define (unzip2 xs)
           (let unzip2 ((xs xs))
             (if (null-list? xs)
                 (values xs xs)
                 (let ((x (car xs)))
                   (receive (a b) (unzip2 (cdr xs))
                            (values (cons (car x) a)
                                    (cons (cadr x) b)))))))

         (define (unzip3 xs)
           (let unzip3 ((xs xs))
             (if (null-list? xs)
                 (values xs xs xs)
                 (let ((x (car xs)))
                   (receive (a b c) (unzip3 (cdr xs))
                            (values (cons (car x) a)
                                    (cons (cadr x) b)
                                    (cons (caddr x) c)))))))

         (define (unzip4 xs)
           (let unzip4 ((xs xs))
             (if (null-list? xs)
                 (values xs xs xs xs)
                 (let ((x (car xs)))
                   (receive (a b c d) (unzip4 (cdr xs))
                            (values (cons (car x) a)
                                    (cons (cadr x) b)
                                    (cons (caddr x) c)
                                    (cons (cadddr x) d)))))))

         (define (unzip5 xs)
           (let unzip5 ((xs xs))
             (if (null-list? xs)
                 (values xs xs xs xs xs)
                 (let ((x (car xs)))
                   (receive (a b c d e) (unzip5 (cdr xs))
                            (values (cons (car x) a)
                                    (cons (cadr x) b)
                                    (cons (caddr x) c)
                                    (cons (cadddr x) d)
                                    (cons (car (cddddr x)) e)))))))

         (define (count satisfy? x . xs)
           (if (pair? xs)
               (let recur ((x x)
                           (xs xs)
                           (i 0))
                 (if (null-list? x)
                     i
                     (receive (as ds) (%cars+cdrs xs)
                       (if (null? as) i
                           (recur (cdr x)
                                  ds
                                  (if (apply satisfy? (car x) as)
                                      (+ i 1)
                                      i))))))
               (let recur ((x x)
                           (i 0))
                 (if (null-list? x)
                     i
                     (recur (cdr x)
                            (if (satisfy? (car x))
                                (+ i 1)
                                i))))))

         (define (fold f z x . xs)
           (if (pair? xs)
               (let recur ((xs (cons x xs))
                           (ans z))
                 (receive (cars+ans cdrs) (%cars+cdrs+ xs ans)
                   (if (null? cars+ans)
                       ans
                       (recur cdrs (apply f cars+ans)))))
               (let recur ((x x)
                           (ans z))
                 (if (null-list? x)
                     ans
                     (recur (cdr x)
                           (f (car x) ans))))))

         (define (unfold p f g seed . generate)
           (if (pair? generate)
               (let ((generate (car generate)))
                 (let recur ((seed seed))
                   (if (p seed)
                       (generate seed)
                       (cons (f seed)
                             (recur (g seed))))))
               (let recur ((seed seed))
                 (if (p seed)
                     '()
                     (cons (f seed)
                           (recur (g seed)))))))

         (define (pair-fold f z x . xs)
           (if (pair? xs)
               (let recur ((xs (cons x xs))
                           (ans z))
                 (let ((tails (%cdrs xs)))
                   (if (null? tails)
                       ans
                       (recur tails (apply f (append! xs (list ans)))))))
               (let recur ((x x)
                           (ans z))
                 (if (null-list? x)
                     ans
                     (let ((tail (cdr x)))
                       (recur tail (f x ans)))))))

         (define (reduce f ridentity x)
           (if (null-list? x)
               ridentity
               (fold f (car x) (cdr x))))

         (define (fold-right f z x . xs)
           (if (pair? xs)
               (let recur ((xs (cons x xs)))
                 (let ((cdrs (%cdrs xs)))
                   (if (null? cdrs)
                       z
                       (apply f (%cars+ xs (recur cdrs))))))
               (let recur ((xs x))
                 (if (null-list? xs)
                     z
                     (let ((x (car xs)))
                       (f x (recur (cdr xs))))))))

         (define (unfold-right p f g seed . tail)
           (let recur ((seed seed)
                       (ans (if (pair? tail)
                                (car tail)
                                '())))
             (if (p seed)
                 ans
                 (recur (g seed)
                        (cons (f seed) ans)))))

         (define (pair-fold-right f z x . xs)
           (if (pair? xs)
               (let recur ((xs (cons x xs)))
                 (let ((cdrs (%cdrs xs)))
                   (if (null? cdrs)
                       z
                       (apply f (append! xs (list (recur cdrs)))))))
               (let recur ((x x))
                 (if (null-list? x)
                     z
                     (f x (recur (cdr x)))))))

         (define (reduce-right f ridentity xs)
           (if (null-list? xs)
               ridentity
               (let reduce-right ((x (car xs))
                                  (xs (cdr xs)))
                 (if (pair? xs)
                     (f x (reduce-right (car xs)
                                        (cdr xs)))
                     x))))

         (define (append-map f x . xs)
           (%append-map append-map append f x xs))

         (define (append-map! f x . xs)
           (%append-map append-map! append! f x xs))

         (define (%append-map who appender f x xs)
           (if (pair? xs)
               (receive (cars cdrs) (%cars+cdrs (cons x xs))
                 (if (null? cars)
                     '()
                     (let recur ((cars cars)
                                 (cdrs cdrs))
                       (let ((vals (apply f cars)))
                         (receive (cars2 cdrs2) (%cars+cdrs cdrs)
                           (if (null? cars2)
                               vals
                               (appender vals (recur cars2 cdrs2))))))))
               (if (null-list? x)
                   '()
                   (let recur ((x (car x))
                               (xs (cdr x)))
                     (let ((vals (f x)))
                       (if (null-list? xs)
                           vals
                           (appender vals
                                     (recur (car xs)
                                            (cdr xs)))))))))

         (define (map! f x . xs)
           (if (pair? xs)
               (let recur ((x x)
                           (xs xs))
                 (if (not (null-list? x))
                     (receive (heads tails) (%cars+cdrs/no-test xs)
                       (set-car! x (apply f (car x) heads))
                       (recur (cdr x)
                              tails))))
               (pair-for-each (lambda (pair)
                                (set-car! pair (f (car pair))))
                              x))
           x)

         (define (pair-for-each f x . xs)
           (if (pair? xs)
               (let recur ((xs (cons x xs)))
                 (let ((tails (%cdrs xs)))
                   (if (pair? tails)
                       (begin (apply f xs)
                              (recur tails)))))
               (let recur ((x x))
                 (if (not (null-list? x))
                     (let ((tail (cdr x)))
                       (f x)
                       (recur tail))))))

         (define (filter-map f x . xs)
           (if (pair? xs)
               (let recur ((xs (cons x xs)))
                 (receive (cars cdrs) (%cars+cdrs xs)
                   (if (pair? cars)
                       (cond ((apply f cars) => (lambda (x) (cons x (recur cdrs))))
                             (else (recur cdrs)))
                       '())))
               (let recur ((x x))
                 (if (null-list? x) x
                     (let ((tail (recur (cdr x))))
                       (cond ((f (car x)) => (lambda (x) (cons x tail)))
                             (else tail)))))))

         (define (map-in-order f x . xs)
           (if (pair? xs)
               (let recur ((xs (cons x xs)))
                 (receive (cars cdrs) (%cars+cdrs xs)
                   (if (pair? cars)
                       (let ((x (apply f cars)))
                         (cons x (recur cdrs)))
                       '())))
               (let recur ((x x))
                 (if (null-list? x) x
                     (let ((tail (cdr x))
                           (x (f (car x))))
                       (cons x (recur tail)))))))

         (define (filter satisfy? x)
           (let recur ((x x))
             (if (null-list? x)
                 x
                 (let ((head (car x))
                       (tail (cdr x)))
                   (if (satisfy? head)
                       (let ((new-tail (recur tail)))
                         (if (eq? tail new-tail)
                             x
                             (cons head new-tail)))
                       (recur tail))))))

         (define (filter! satisfy? xs)
           (let recur ((xs xs))
             (if (pair? xs)
                 (cond ((satisfy? (car xs))
                        (set-cdr! xs (recur (cdr xs)))
                        xs)
                       (else (recur (cdr xs))))
                 xs)))

         (define (partition satisfy? xs)
           (let recur ((xs xs))
             (if (null-list? xs)
                 (values xs xs)
                 (let ((x (car xs)))
                   (receive (a b) (recur (cdr xs))
                     (if (satisfy? x)
                         (values (if (pair? b)
                                     (cons x a)
                                     xs)
                                 b)
                         (values a
                                 (if (pair? a)
                                     (cons x b)
                                     xs))))))))

         (define (partition! satisfy? xs)
           (if (null-list? xs)
               (values xs xs)
               (letrec ((scan-in (lambda (in-prev out-prev xs)
                                   (let recur ((in-prev in-prev)
                                               (xs xs))
                                     (if (pair? xs)
                                         (if (satisfy? (car xs))
                                             (recur xs (cdr xs))
                                             (begin (set-cdr! out-prev xs)
                                                    (scan-out in-prev xs (cdr xs))))
                                         (set-cdr! out-prev xs)))))
                        (scan-out (lambda (in-prev out-prev xs)
                                    (let recur ((out-prev out-prev)
                                                (xs xs))
                                      (if (pair? xs)
                                          (if (satisfy? (car xs))
                                              (begin (set-cdr! in-prev xs)
                                                     (scan-in xs out-prev (cdr xs)))
                                              (recur xs (cdr xs)))
                                          (set-cdr! in-prev xs))))))
                 (if (satisfy? (car xs))
                     (let recur ((prev-l xs)
                                 (l (cdr xs)))
                       (cond ((not (pair? l))
                              (values xs l))
                             ((satisfy? (car l))
                              (recur l (cdr l)))
                             (else (scan-out prev-l l (cdr l))
                                   (values xs l))))
                     (let recur ((prev-l xs)
                                 (l (cdr xs)))
                       (cond ((not (pair? l))
                              (values l xs))
                             ((satisfy? (car l))
                              (scan-in l prev-l (cdr l))
                              (values l xs))
                             (else (recur l (cdr l)))))))))

         (define (remove satisfy? xs)
           (filter (lambda (x)
                     (not (satisfy? x)))
                   xs))

         (define (remove! satisfy? xs)
           (filter! (lambda (x)
                      (not (satisfy? x)))
                    xs))

         (define (find satisfy? xs)
           (cond ((find-tail satisfy? xs) => car)
                 (else #f)))

         (define (find-tail satisfy? xs)
           (let recur ((xs xs))
             (and (not (null-list? xs))
                  (if (satisfy? (car xs))
                      xs
                      (recur (cdr xs))))))

         (define (any satisfy? x . xs)
           (if (pair? xs)
               (receive (cars cdrs) (%cars+cdrs (cons x xs))
                 (and (pair? cars)
                      (let recur ((cars cars)
                                  (cdrs cdrs))
                        (receive (next-cars next-cdrs) (%cars+cdrs cdrs)
                          (if (pair? next-cars)
                              (or (apply satisfy? cars)
                                  (recur next-cars
                                         next-cdrs))
                              (apply satisfy? cars))))))
               (and (not (null-list? x))
                    (let recur ((head (car x))
                                (tail (cdr x)))
                      (if (null-list? tail)
                          (satisfy? head)
                          (or (satisfy? head)
                              (recur (car tail)
                                     (cdr tail))))))))

         (define (every satisfy? x . xs)
           (if (pair? xs)
               (receive (heads tails) (%cars+cdrs (cons x xs))
                 (or (not (pair? heads))
                     (let recur ((heads heads)
                                 (tails tails))
                       (receive (next-heads next-tails) (%cars+cdrs tails)
                         (if (pair? next-heads)
                             (and (apply satisfy? heads)
                                  (recur next-heads next-tails))
                             (apply satisfy? heads))))))
               (or (null-list? x)
                   (let recur ((head (car x))
                               (tail (cdr x)))
                     (if (null-list? tail)
                         (satisfy? head)
                         (and (satisfy? head)
                              (recur (car tail)
                                     (cdr tail))))))))

         (define (list-index satisfy? x . xs)
           (if (pair? xs)
               (let recur ((xs (cons x xs))
                           (n 0))
                 (receive (heads tails) (%cars+cdrs xs)
                   (and (pair? heads)
                        (if (apply satisfy? heads)
                            n
                            (recur tails (+ n 1))))))
               (let recur ((xs x)
                           (n 0))
                 (and (not (null-list? xs))
                      (if (satisfy? (car xs))
                          n
                          (recur (cdr xs)
                                 (+ n 1)))))))

         (define (take-while satisfy? xs)
           (let recur ((xs xs))
             (if (null-list? xs)
                 '()
                 (let ((x (car xs)))
                   (if (satisfy? x)
                       (cons x (recur (cdr xs)))
                       '())))))

         (define (take-while! satisfy? xs)
           (if (or (null-list? xs)
                   (not (satisfy? (car xs))))
               '()
               (begin (let recur ((prev xs)
                                  (rest (cdr xs)))
                        (if (pair? rest)
                            (let ((x (car rest)))
                              (if (satisfy? x)
                                  (recur rest (cdr rest))
                                  (set-cdr! prev '())))))
                      xs)))

         (define (drop-while satisfy? xs)
           (let recur ((xs xs))
             (if (null-list? xs)
                 '()
                 (if (satisfy? (car xs))
                     (recur (cdr xs))
                     xs))))

         (define (span satisfy? xs)
           (let recur ((xs xs))
             (if (null-list? xs)
                 (values '() '())
                 (let ((x (car xs)))
                   (if (satisfy? x)
                       (receive (a b) (recur (cdr xs))
                         (values (cons x a) b))
                       (values '() xs))))))

         (define (span! satisfy? xs)
           (if (or (null-list? xs)
                   (not (satisfy? (car xs))))
               (values '() xs)
               (let ((suffix (let recur ((prev xs)
                                         (rest (cdr xs)))
                               (if (null-list? rest)
                                   rest
                                   (let ((x (car rest)))
                                     (if (satisfy? x)
                                         (recur rest (cdr rest))
                                         (begin (set-cdr! prev '())
                                                rest)))))))
                 (values xs suffix))))

         (define (break break? x)
           (span (lambda (x)
                   (not (break? x)))
                 x))

         (define (break! break? x)
           (span! (lambda (x)
                    (not (break? x)))
                  x))

         (define (delete x xs . x=?)
           (let ((x=? (if (pair? x=?)
                          (car x=?)
                          equal?)))
             (filter (lambda (y)
                       (not (x=? x y)))
                     xs)))

         (define (delete! x xs . x=?)
           (let ((x=? (if (pair? x=?)
                          (car x=?)
                          equal?)))
             (filter! (lambda (y)
                        (not (x=? x y)))
                      xs)))

         (define (delete-duplicates xs . x=?)
           (let ((x=? (if (pair? x=?)
                          (car x=?)
                          equal?)))
             (let recur ((x:xs xs))
               (if (null-list? x:xs)
                   '()
                   (let* ((x (car x:xs))
                          (xs (cdr x:xs))
                          (ys (recur (delete x xs x=?))))
                     (if (eq? xs ys)
                         x:xs
                         (cons x ys)))))))

         (define (delete-duplicates! xs . x=?)
           (let ((x=? (if (pair? x=?)
                          (car x=?)
                          equal?)))
             (let recur ((x:xs xs))
               (if (null-list? x:xs)
                   '()
                   (let* ((x (car x:xs))
                          (xs (cdr x:xs))
                          (ys (recur (delete! x xs x=?))))
                     (if (eq? xs ys)
                         x:xs
                         (cons x ys)))))))

         (define (alist-delete key alist . key=?)
           (let ((key=? (if (pair? key=?)
                            (car key=?)
                            equal?)))
             (filter (lambda (x)
                       (not (key=? key (car x))))
                     alist)))

         (define (alist-delete! key alist . key=?)
           (let ((key=? (if (pair? key=?)
                        (car key=?)
                        equal?)))
             (filter! (lambda (x)
                        (not (key=? key (car x))))
                      alist)))

         (define (lset<= x=? . xss)
           (or (not (pair? xss))
               (let recur ((xs (car xss))
                           (xss (cdr xss)))
                 (or (not (pair? xss))
                     (let ((ys (car xss))
                           (xss (cdr xss)))
                       (and (or (eq? xs ys)
                                (%lset2<= x=? xs ys))
                            (recur ys xss)))))))

         (define (lset= x=? . xss)
           (or (not (pair? xss))
               (let recur ((xs (car xss))
                           (xss (cdr xss)))
                 (or (not (pair? xss))
                     (let ((ys (car xss))
                           (xss (cdr xss)))
                       (and (or (eq? xs ys)
                                (and (%lset2<= x=? xs ys)
                                     (%lset2<= x=? ys xs)))
                            (recur ys xss)))))))

         (define (lset-adjoin x=? xs . ys)
           (fold (lambda (y xs)
                   (if (member y xs x=?)
                       xs
                       (cons y xs)))
                 xs
                 ys))

         (define (lset-union x=? . xss)
           (reduce (lambda (xs ys)
                     (cond ((null? xs) ys)
                           ((null? ys) xs)
                           ((eq? xs ys) ys)
                           (else (fold (lambda (x ys)
                                         (if (any (lambda (y)
                                                    (x=? x y))
                                                  ys)
                                             ys
                                             (cons x ys)))
                                       ys
                                       xs))))
                   '()
                   xss))

         (define (lset-union! x=? . xss)
           (reduce (lambda (xs ys)
                     (cond ((null? xs) ys)
                           ((null? ys) xs)
                           ((eq? xs ys) ys)
                           (else (pair-fold (lambda (x:xs ys)
                                              (let ((x (car x:xs)))
                                                (if (any (lambda (y)
                                                           (x=? x y))
                                                         ys)
                                                    ys
                                                    (begin (set-cdr! x:xs ys)
                                                           x:xs))))
                                            ys
                                            xs))))
                   '()
                   xss))

         (define (lset-intersection x=? xs . xss)
           (let ((xss (delete xs xss eq?)))
             (cond ((any null-list? xss) '())
                   ((null? xss) xs)
                   (else (filter (lambda (x)
                                   (every (lambda (xs)
                                            (member x xs x=?))
                                          xss))
                                 xs)))))

         (define (lset-intersection! x=? xs . xss)
           (let ((xss (delete xs xss eq?)))
             (cond ((any null-list? xss) '())
                   ((null? xss) xs)
                   (else (filter! (lambda (x)
                                    (every (lambda (xs)
                                             (member x xs x=?))
                                           xss))
                                  xs)))))

         (define (lset-difference x=? xs . xss)
           (let ((xss (filter pair? xss)))
             (cond ((null? xss) xs)
                   ((memq xs xss) '())
                   (else (filter (lambda (x)
                                   (every (lambda (xs)
                                            (not (member x xs x=?)))
                                          xss))
                                 xs)))))

         (define (lset-difference! x=? xs . xss)
           (let ((xss (filter pair? xss)))
             (cond ((null? xss) xs)
                   ((memq xs xss) '())
                   (else (filter! (lambda (x)
                                    (every (lambda (xs)
                                             (not (member x xs x=?)))
                                           xss))
                                  xs)))))

         (define (lset-xor x=? . xss)
           (reduce (lambda (b a)
                     (receive (a-b a^b) (lset-diff+intersection x=? a b)
                       (cond ((null? a-b) (lset-difference x=? b a))
                             ((null? a^b) (append b a))
                             (else (fold (lambda (x xs)
                                           (if (member x a^b x=?)
                                               xs
                                               (cons x xs)))
                                         a-b
                                         b)))))
                   '()
                   xss))

         (define (lset-xor! x=? . xss)
           (reduce (lambda (b a)
                     (receive (a-b a^b) (lset-diff+intersection! x=? a b)
                       (cond ((null? a-b) (lset-difference! x=? b a))
                             ((null? a^b) (append! b a))
                             (else (pair-fold (lambda (x:xs ys)
                                                (if (member (car x:xs) a^b x=?)
                                                    ys
                                                    (begin (set-cdr! x:xs ys)
                                                           x:xs)))
                                              a-b
                                              b)))))
                   '()
                   xss))

         (define (lset-diff+intersection x=? xs . xss)
           (cond ((every null-list? xss)
                  (values xs '()))
                 ((memq xs xss)
                  (values '() xs))
                 (else (partition (lambda (x)
                                    (not (any (lambda (xs)
                                                (member x xs x=?))
                                              xss)))
                                  xs))))

         (define (lset-diff+intersection! x=? xs . xss)
           (cond ((every null-list? xss)
                  (values xs '()))
                 ((memq xs xss)
                  (values '() xs))
                 (else (partition! (lambda (x)
                                     (not (any (lambda (xs)
                                                 (member x xs x=?))
                                               xss)))
                                   xs)))))

  (begin (define (%cdrs xss)
           (call-with-current-continuation!
             (lambda (abort)
               (let recur ((xss xss))
                 (if (pair? xss)
                     (let ((xs (car xss)))
                       (if (null-list? xs)
                           (abort '())
                           (cons (cdr xs)
                                 (recur (cdr xss)))))
                     '())))))

         (define (%cars+ xss cars)
           (let recur ((xss xss))
             (if (pair? xss)
                 (cons (caar xss)
                       (recur (cdr xss)))
                 (list cars))))

         (define (%cars+cdrs xss)
           (call-with-current-continuation!
             (lambda (abort)
               (let recur ((xss xss))
                 (if (pair? xss)
                     (receive (xs xss) (car+cdr xss)
                       (if (null-list? xs)
                           (abort '()
                                  '())
                           (receive (a d) (car+cdr xs)
                             (receive (cars cdrs) (recur xss)
                               (values (cons a cars)
                                       (cons d cdrs))))))
                     (values '()
                             '()))))))

         (define (%cars+cdrs+ xss cars)
           (call-with-current-continuation!
             (lambda (abort)
               (let recur ((xss xss))
                 (if (pair? xss)
                     (receive (xs xss) (car+cdr xss)
                       (if (null-list? xs)
                           (abort '()
                                  '())
                           (receive (a d) (car+cdr xs)
                             (receive (cars cdrs) (recur xss)
                               (values (cons a cars)
                                       (cons d cdrs))))))
                     (values (list cars)
                             '()))))))

         (define (%cars+cdrs/no-test xss)
           (let recur ((xss xss))
             (if (pair? xss)
                 (receive (xs xss) (car+cdr xss)
                   (receive (a d) (car+cdr xs)
                     (receive (cars cdrs) (recur xss)
                       (values (cons a cars)
                               (cons d cdrs)))))
                 (values '()
                         '()))))

         (define (%lset2<= x=? xs ys)
           (every (lambda (x)
                    (member x ys x=?))
                  xs))))
