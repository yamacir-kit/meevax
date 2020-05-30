(check-set-mode! 'report)

; ==== 4.1. Primitive expression types =========================================

; ---- 4.1.1 Variable references -----------------------------------------------

(define x 28)
(check x => 28)

; ---- 4.1.2 Literal expressions -----------------------------------------------

(check (quote a) => a)

; (check (quote #(a b c)) => #(a b c))

(check (quote (+ 1 2)) => (+ 1 2))

(check 'a => a)

; (check '#(a b c) => #(a b c))

(check '() => ())

; ==== REPORT ==================================================================

(check-report)

(exit (if (check-passed? check::correct)
          exit-success
          exit-failure))
