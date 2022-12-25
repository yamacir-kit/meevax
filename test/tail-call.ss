(import (meevax experimental) ; disassemble
        (scheme base)
        (scheme write)
        (scheme process-context)
        (srfi 78)
        )

(define (object->string x)
  (parameterize ((current-output-port (open-output-string "")))
    (write x)
    (get-output-string (current-output-port))))

(define (f1)
  (car '(a b)))

(disassemble f1)

(check (object->string (car f1)) => "(load-constant ((a b)) \
                                      load-absolute #,(identity car) \
                                      tail-call)")

(check-report)

(exit (check-passed? 1))
