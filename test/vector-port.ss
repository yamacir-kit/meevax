(import (scheme base)
        (scheme process-context)
        (srfi 78))

(let ((output (open-output-bytevector)))
  (check (get-output-bytevector output) => #u8())
  (write-u8 0 output)
  (check (get-output-bytevector output) => #u8(0))
  (write-u8 1 output)
  (check (get-output-bytevector output) => #u8(0 1))
  (write-u8 2 output)
  (check (get-output-bytevector output) => #u8(0 1 2))

  (let ((input (open-input-bytevector (get-output-bytevector output))))
    (check (u8-ready? input) => #t)
    (check (peek-u8 input) => 0)
    (check (read-u8 input) => 0)

    (check (u8-ready? input) => #t)
    (check (peek-u8 input) => 1)
    (check (read-u8 input) => 1)

    (check (u8-ready? input) => #t)
    (check (peek-u8 input) => 2)
    (check (read-u8 input) => 2)

    (check (u8-ready? input) => #f)
    (check (peek-u8 input) => (eof-object))
    (check (read-u8 input) => (eof-object))))

(check-report)

(exit (check-passed? 16))
