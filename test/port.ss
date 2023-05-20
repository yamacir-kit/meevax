(import (scheme base)
        (scheme file)
        (scheme write)
        (scheme process-context)
        (srfi 78))

(define (display-file path)
  (parameterize ((current-input-port (open-input-file path)))
    (let recur ((line (read-line)))
      (if (not (eof-object? line))
          (begin (display line)
                 (newline)
                 (recur (read-line)))))))

(define path "/tmp/hoge.txt")

(check (file-exists? path) => #f)

(let ((output-port (open-binary-output-file path)))
  (check (output-port? output-port) => #t)
  (check (output-port-open? output-port) => #t)
  (write-bytevector #u8(65 66 67) output-port)
  (write-u8 (char->integer #\newline) output-port)
  (write-bytevector #u8(68 69 70) output-port)
  (write-u8 (char->integer #\newline) output-port)
  (flush-output-port output-port)
  (close-output-port output-port)
  (check (output-port-open? output-port) => #f))

(display-file path)

(let ((input-port (open-input-file path)))
  (check (input-port? input-port) => #t)
  (check (input-port-open? input-port) => #t)
  (check (char-ready? input-port) => #t)
  (check (read-char input-port) => #\A)
  (check (read-char input-port) => #\B)
  (check (read-char input-port) => #\C)
  (check (peek-char input-port) => #\newline)
  (check (read-line input-port) => "")
  (check (read-line input-port) => "DEF")
  (check (char-ready? input-port) => #t)
  (check (peek-char input-port) => (eof-object))
  (check (read-char input-port) => (eof-object))
  (close-input-port input-port)
  (check (input-port-open? input-port) => #f))

(display-file path)

(let ((output-port (open-output-file path)))
  (check (output-port? output-port) => #t)
  (check (output-port-open? output-port) => #t)
  (write-string "GHI" output-port)
  (newline output-port)
  (write-string "JKL" output-port)
  (newline output-port)
  (flush-output-port output-port)
  (close-output-port output-port)
  (check (output-port-open? output-port) => #f))

(display-file path)

(let ((input-port (open-binary-input-file path)))
  (check (input-port? input-port) => #t)
  (check (input-port-open? input-port) => #t)
  (check (read-bytevector 100 input-port) => #u8(71 72 73 10
                                                 74 75 76 10))
  (close-input-port input-port)
  (check (input-port-open? input-port) => #f))

(display-file path)

(check (file-exists? path) => #t)
(delete-file path)
(check (file-exists? path) => #f)

(check-report)

(exit (check-passed? 26))
