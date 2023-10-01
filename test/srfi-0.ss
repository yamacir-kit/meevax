(import (scheme base)
        (scheme process-context)
        (scheme write)
        (srfi 78))

(check (cond-expand
         (r5rs 'r5rs)
         (r6rs 'r6rs)
         (r7rs 'r7rs)
         (else 'unknown))
  => 'r7rs)

(check (cond-expand
         ((library (scheme base)) 'supported)
         (else 'unsupported))
  => 'supported)

(check (cond-expand
         ((and exact-closed exact-complex ratios) #t)
         (else #f))
  => #t)

(check (cond-expand
         ((or r5rs r6rs r7rs) 'standard)
         (else 'unknown))
  => 'standard)

(check (cond-expand
         ((not r7rs) 'not-r7rs)
         (else 'r7rs))
  => 'r7rs)

(check (cond-expand
         ((and (library (scheme base))
               (library (scheme case-lambda))
               (library (scheme char))
               (library (scheme complex))
               (library (scheme cxr))
               (library (scheme eval))
               (library (scheme file))
               (library (scheme inexact))
               (library (scheme lazy))
               (library (scheme load))
               (library (scheme process-context))
               (library (scheme read))
               (library (scheme repl))
               (library (scheme time))
               (library (scheme write))
               (library (scheme r5rs)))
          'all-r7rs-small-library-supported)
         (else #f))
  => 'all-r7rs-small-library-supported)

(check-report)

(exit (check-passed? 6))
