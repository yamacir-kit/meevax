(define port (open-input-string "lambdaλラムダ"))

(write (get-char! port)) ; #\l
(write (get-char! port)) ; #\a
(write (get-char! port)) ; #\m
(write (get-char! port)) ; #\b
(write (get-char! port)) ; #\d
(write (get-char! port)) ; #\a
(newline)

(write (get-char! port)) ; #\λ or #\x03bb
(newline)

(write (get-char! port)) ; #\ラ
(write (get-char! port)) ; #\ム
(write (get-char! port)) ; #\ダ
(newline)
