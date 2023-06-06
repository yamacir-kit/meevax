(import (scheme base)
        (scheme file)
        (scheme write)
        (scheme process-context)
        (srfi 78))

(define-library (test 1)
  (import (scheme base))
  (export f (rename g h))
  (begin (define (f) 'f)
         (define (g) 'g)))

(import (test 1))

(check (f) => 'f)

(check (h) => 'g)

(check (file-exists? "/tmp/hoge.ss") => #f)
(check (file-exists? "/tmp/fuga.ss") => #f)

(let ((hoge.ss (open-output-file "/tmp/hoge.ss")))
  (check (output-port-open? hoge.ss) => #t)
  (write '(define-syntax swap!
            (syntax-rules ()
              ((swap! a b)
               (let ((tmp a))
                 (set! a b)
                 (set! b tmp)))))
         hoge.ss)
  (flush-output-port hoge.ss)
  (close-output-port hoge.ss)
  (check (output-port-open? hoge.ss) => #f)
  )

(let ((fuga.ss (open-output-file "/tmp/fuga.ss")))
  (check (output-port-open? fuga.ss) => #t)
  (write '(define x 1) fuga.ss)
  (write '(DEFINE Y 2) fuga.ss)
  (write '(swap! x y) fuga.ss)
  (flush-output-port fuga.ss)
  (close-output-port fuga.ss)
  (check (output-port-open? fuga.ss) => #f)
  )

(check (file-exists? "/tmp/hoge.ss") => #t)
(check (file-exists? "/tmp/fuga.ss") => #t)

(include-ci "/tmp/hoge.ss"
            "/tmp/fuga.ss")

(check (cons x y) => '(2 . 1))

(delete-file "/tmp/hoge.ss")
(delete-file "/tmp/fuga.ss")

(check (file-exists? "/tmp/hoge.ss") => #f)
(check (file-exists? "/tmp/fuga.ss") => #f)

(check-report)

(exit (check-passed? 13))
