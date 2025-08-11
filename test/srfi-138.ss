(import (scheme base)
        (scheme process-context)
        (srfi 78)
        (test write)
        )

(check (procedure? print) => #t)

(check-report)

(exit (check-passed? 1))
