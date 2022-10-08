(import (scheme base)
        (scheme eval)
        (scheme process-context)
        (scheme write)
        (srfi 78))

(check (call/cc (lambda (return)
                  (with-exception-handler
                    (lambda (x)
                      (return "addition aborted"))
                    (lambda ()
                      (+ 1 "2" 3))))) => "addition aborted")

(check (call/cc (lambda (return)
                  (with-exception-handler
                    (lambda (x)
                      (return "addition aborted"))
                    (lambda ()
                      (eval '(+ 1 "2" 3)
                            (environment '(scheme base))))))) => "addition aborted")

(check (call/cc (lambda (return)
                  (with-exception-handler
                    (lambda (x)
                      (return "addition aborted 1"))
                    (lambda ()
                      (eval '(call/cc (lambda (return)
                                        (with-exception-handler
                                          (lambda (e)
                                            (return "addition aborted 2"))
                                          (lambda ()
                                            (+ 1 "2" 3)))))
                            (environment '(scheme base))))))) => "addition aborted 2")

(check (call/cc (lambda (return)
                  (with-exception-handler
                    (lambda (x)
                      (display "receive!")
                      (newline)
                      (return "addition aborted 1"))
                    (lambda ()
                      (eval '(call/cc (lambda (return)
                                        (with-exception-handler
                                          (lambda (e)
                                            (display "receive, but ignore!")
                                            (newline))
                                          (lambda ()
                                            (+ 1 "2" 3)))))
                            (environment '(scheme base)
                                         '(scheme write))))))) => "addition aborted 1")

(check-report)

(exit (check-passed? 4))
