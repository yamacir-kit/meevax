(import (scheme base)
        (scheme process-context)
        (srfi 78))

(define a 100)
(define b 200)

(let ()
  (define a 1)
  (define b 2)
  (check (+ a b) => 3))

(define hoge "global:hoge")
(define fuga "global:fuga")

(let ((define cons))
  (check (define hoge "local:hoge") => ("global:hoge" . "local:hoge"))
  (check (define fuga "local:fuga") => ("global:fuga" . "local:fuga"))
  (check hoge => "global:hoge")
  (check fuga => "global:fuga"))

(check-report)

(exit (check-passed? 5))
