; 6.7 String

"The word \"recusion\" has many meanings."
"Another example:\ntwo lines of text"
"Here's text \
  containing just one line"
"\x03B1; is named GREEK SMALL LETTER ALPHA."

(define (f) (make-string 3 #\*))
(define (g) "***")
(string-set! (f) 0 #\?); -> #<unspecified>
(string-set! (g) 0 #\?); -> #<error>
(string-set! (symbol->string 'immutable) 0 #\?); -> #<error>

(define a "12345")
(define b (string-copy "abcde"))
(string-copy! b 1 a 0 2)
b; -> "a12de"

