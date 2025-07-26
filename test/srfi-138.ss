(import (scheme base)
        (scheme process-context)
        (srfi 78)
        (test print)
        )

(check (procedure? print) => #t)

(check-report)

(exit (check-passed? 1))
