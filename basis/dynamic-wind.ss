; https://www.cs.hmc.edu/~fleck/envision/scheme48/meeting/node7.html

(define dynamic-extents '())

(define (dynamic-wind before thunk after)
  (before)
  (set! dynamic-extents (cons (cons before after) dynamic-extents))
  ((lambda (result) ; TODO let-values
     (set! dynamic-extents (cdr dynamic-extents))
     (after)
     result) ; TODO (apply values result)
   (thunk)))

(define call-with-current-continuation ; overwrite
  ((lambda (call/cc)
     (lambda (procedure)

       (define (windup! from to)
         (set! dynamic-extents from)

         (if (eq? from to) #t
         (if (null? from)
             (begin (windup! from (cdr to))
                    ((caar to)))
         (if (null? to)
             (begin ((cdar from))
                    (windup! (cdr from) to))
             (begin ((cdar from))
                    (windup! (cdr from) (cdr to))
                    ((caar to))))))

         (set! dynamic-extents to))

       ((lambda (current-dynamic-extents)
          (call/cc (lambda (k1)
                     (procedure (lambda (k2)
                                  (windup! dynamic-extents current-dynamic-extents)
                                  (k1 k2))))))
        dynamic-extents)))
   (lambda (procedure)
     (call-with-current-continuation procedure))))
