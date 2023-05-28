(import (scheme base)
        (scheme process-context)
        (srfi 78)
        )

(let ((japanese "日本語"))
  (check (string-length japanese) => 3)
  (let ((input-port (open-input-string japanese)))
    (check (peek-char input-port) => #\日)
    (check (read-char input-port) => #\日)
    (check (peek-char input-port) => #\本)
    (check (read-char input-port) => #\本)
    (check (peek-char input-port) => #\語)
    (check (read-char input-port) => #\語)))

(check-report)

(exit (check-passed? 7))
