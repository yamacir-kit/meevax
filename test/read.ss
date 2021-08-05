; (import (scheme base)
;         (scheme file)
;         (scheme read)
;         (srfi 78))

(let ((lambda.ss "/home/yamasa/.meevax/test/lambda.ss"))
  (check (read (open-input-file lambda.ss)) => #\x03bb)
  (check (read-char (open-input-file lambda.ss)) => #\#)
  ; (check (read-string 1 (open-input-file lambda.ss)) => "#")
  )

(let ((newline.ss "/home/yamasa/.meevax/test/newline.ss"))
  (check (read (open-input-file newline.ss)) => '\n)
  (check (symbol? '\n) => #t)
  (check (read-char (open-input-file newline.ss)) => #\\)
  ; (check (read-string 1 (open-input-file newline.ss)) => "\\")
  )


