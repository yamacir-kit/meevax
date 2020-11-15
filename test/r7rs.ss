; ---- SRFI-78 -----------------------------------------------------------------

(check-set-mode! 'report)

; ---- 2.1. Identifiers --------------------------------------------------------

(check (symbol? '...) => #t)
(check (symbol? '+) => #t)
(check (symbol? '+soup+) => #t)
(check (symbol? '<=?) => #t)
(check (symbol? '->string) => #t)
(check (symbol? 'a34kTMNs) => #t)
(check (symbol? 'lambda) => #t)
(check (symbol? 'list->vector) => #t)
(check (symbol? 'q) => #t)
(check (symbol? 'V17a) => #t)
; (check (symbol? |two words|) => #t)
; (check (symbol? |two\x20;words|) => #t)
(check (symbol? 'the-word-recursion-has-many-meanings) => #t)

#!fold-case
#!no-fold-case

; ---- 2.2. Whitespace and comments --------------------------------------------

; #|
;   The FACT procedure computes the factorial
;   of a non-negative integer.
; |#
(define fact
  (lambda (n)
    (if (= n 0)
        #;(= n 1)
        1
        ;Base case: return 1
        (* n (fact (- n 1))))))

; ---- make-parameter ----------------------------------------------------------



; ---- SRFI-78 -----------------------------------------------------------------

(check-report)

(exit (if (check-passed? check::correct) exit-success exit-failure))
