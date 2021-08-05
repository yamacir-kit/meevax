(define port (open-input-string "lambdaλラムダ"))

(write (::read-char port)) ; #\l
(write (::read-char port)) ; #\a
(write (::read-char port)) ; #\m
(write (::read-char port)) ; #\b
(write (::read-char port)) ; #\d
(write (::read-char port)) ; #\a
(newline)

(write (::read-char port)) ; #\λ or #\x03bb
(newline)

(write (::read-char port)) ; #\ラ
(write (::read-char port)) ; #\ム
(write (::read-char port)) ; #\ダ
(newline)
