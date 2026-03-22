(define-library (srfi 141) ; Based on Chibi-Scheme's lib/scheme/division.scm (Public Domain).
  (import (scheme r5rs))

  (export floor/
          floor-quotient
          floor-remainder
          ceiling/
          ceiling-quotient
          ceiling-remainder
          truncate/
          truncate-quotient
          truncate-remainder
          round/
          round-quotient
          round-remainder
          euclidean/
          euclidean-quotient
          euclidean-remainder
          balanced/
          balanced-quotient
          balanced-remainder)

  (begin (define (floor-quotient x y)
           (floor (/ x y)))

         (define floor-remainder modulo)

         (define (floor/ x y)
           (values (floor-quotient x y)
                   (floor-remainder x y)))

         (define (ceiling-quotient n d)
           (let ((q (ceiling (/ n d))))
             (if (and (exact? n)
                      (exact? d))
                 (inexact->exact q)
                 q)))

         (define (ceiling-remainder n d)
           (- n (* d (ceiling-quotient n d))))

         (define (ceiling/ n d)
           (values (ceiling-quotient n d)
                   (ceiling-remainder n d)))

         (define truncate-quotient quotient)

         (define truncate-remainder remainder)

         (define (truncate/ x y)
           (values (truncate-quotient x y)
                   (truncate-remainder x y)))

         (define (round-quotient n d)
           (let ((q (round (/ n d))))
             (if (and (exact? n)
                      (exact? d))
                 (inexact->exact q)
                 q)))

         (define (round-remainder n d)
           (- n (* d (round-quotient n d))))

         (define (round/ n d)
           (values (round-quotient n d)
                   (round-remainder n d)))

         (define (euclidean-quotient n d)
           (if (< 0 d)
               (floor-quotient n d)
               (ceiling-quotient n d)))

         (define (euclidean-remainder n d)
           (- n (* d (euclidean-quotient n d))))

         (define (euclidean/ n d)
           (values (euclidean-quotient n d)
                   (euclidean-remainder n d)))

         (define (balanced-remainder n d)
           (let ((r (euclidean-remainder n d))
                 (d/2 (abs (/ d 2))))
             (cond ((< r (- d/2))
                    (+ r (abs d)))
                   ((<= d/2 r)
                    (- r (abs d)))
                   (else r))))

         (define (balanced-quotient n d)
           (quotient (- n (balanced-remainder n d)) d))

         (define (balanced/ n d)
           (values (balanced-quotient n d)
                   (balanced-remainder n d)))))
