(import (meevax environment)
        (scheme base)
        (scheme process-context)
        (scheme read)
        (scheme repl)
        (scheme write)
        (srfi 78)
        )

(define (external-representation-of object)
  (parameterize ((current-output-port (open-output-string)))
    (write object)
    (read (open-input-string (get-output-string (current-output-port))))))

(let* ((e1-0 '(+ 1 2 3))
       (e1-1 (expand   e1-0 (interaction-environment)))
       (e1-2 (convert  e1-1 (interaction-environment)))
       (e1-3 (generate e1-2 (interaction-environment))))
  (check (external-representation-of e1-0) => e1-0)
  (check (external-representation-of e1-1) => '(+ 1 2 3))
  (check (external-representation-of e1-2) => '(+ #kontinuation 1 2 3))
  (check (external-representation-of e1-3) =>
    '(load-constant 3
      load-constant 2
      load-constant 1
      load-constant #k
      load-absolute +
      tail-call)))

(check-report)

(exit (check-passed? 4))
