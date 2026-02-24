(import (scheme base)
        (scheme cxr)
        (scheme process-context)
        (scheme write)
        (srfi 1)
        (srfi 78)
        (test write))

(define a       (circular-list 'a         ))
(define b       (circular-list 'b         ))
(define c       (circular-list 'c         ))
(define a-b     (circular-list 'a 'b      ))
(define a-b-c   (circular-list 'a 'b 'c   ))
(define a-b-c-d (circular-list 'a 'b 'c 'd))

(define list-a-a   (list a a)         )
(define list-a-b   (list a b)         )
(define list-a-b-c (list a (list b c)))

(define quote-a-b-c       '#0=(a b c . #0#)        )
(define quote-a-b-c-d-e-f '#0=(a b c (d e f . #0#)))

(check (circular-list? a      ) => #t)
(check (circular-list? b      ) => #t)
(check (circular-list? c      ) => #t)
(check (circular-list? a-b    ) => #t)
(check (circular-list? a-b-c  ) => #t)
(check (circular-list? a-b-c-d) => #t)

(check (circular-list? list-a-a) => #f)
(check (circular-list? list-a-b) => #f)

(check (circular-list? quote-a-b-c      ) => #t)
(check (circular-list? quote-a-b-c-d-e-f) => #f)

(define (written-string write x)
  (parameterize ((current-output-port (open-output-string)))
    (write x)
    (get-output-string (current-output-port))))

(check (written-string write        a      ) => "#1=(a . #1#)"      )
(check (written-string write-shared a      ) => "#1=(a . #1#)"      )
(check (written-string write        a-b    ) => "#1=(a b . #1#)"    )
(check (written-string write-shared a-b    ) => "#1=(a b . #1#)"    )
(check (written-string write        a-b-c  ) => "#1=(a b c . #1#)"  )
(check (written-string write-shared a-b-c  ) => "#1=(a b c . #1#)"  )
(check (written-string write        a-b-c-d) => "#1=(a b c d . #1#)")
(check (written-string write-shared a-b-c-d) => "#1=(a b c d . #1#)")

(check (written-string write        list-a-a  ) => "(#1=(a . #1#) #1#)"                        )
(check (written-string write-shared list-a-a  ) => "(#1=(a . #1#) #1#)"                        )
(check (written-string write        list-a-b  ) => "(#1=(a . #1#) #2=(b . #2#))"               )
(check (written-string write-shared list-a-b  ) => "(#1=(a . #1#) #2=(b . #2#))"               )
(check (written-string write        list-a-b-c) => "(#1=(a . #1#) (#2=(b . #2#) #3=(c . #3#)))")
(check (written-string write-shared list-a-b-c) => "(#1=(a . #1#) (#2=(b . #2#) #3=(c . #3#)))")

(check (written-string write        quote-a-b-c      ) => "#1=(a b c . #1#)"        )
(check (written-string write-shared quote-a-b-c      ) => "#1=(a b c . #1#)"        )
(check (written-string write-shared quote-a-b-c-d-e-f) => "#1=(a b c (d e f . #1#))")

(check-report)

(exit (check-passed? 27))
