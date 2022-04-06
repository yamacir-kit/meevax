(define value 42)

; ------------------------------------------------------------------------------

(check (identifier? (identifier 'value)) => #t)

(check (identifier? 'value) => #t)

(check (identifier? 3) => #f)

; ------------------------------------------------------------------------------

(check (identifier->symbol (identifier 'value)) => value)

(check-report)

(exit (check-passed? check:correct))
