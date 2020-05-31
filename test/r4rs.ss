(check-set-mode! 'report)


; ==== 4.1. Primitive expression types =========================================

; ---- 4.1.1. Variable references ----------------------------------------------

(define x 28) ; => unspecified

(check x => 28)

; ---- 4.1.2. Literal expressions ----------------------------------------------

(check (quote a) => a)
; (check (quote #(a b c)) => #(a b c))
(check (quote (+ 1 2)) => (+ 1 2))

(check 'a => a)
; (check '#(a b c) => #(a b c))
(check '(+ 1 2) => (+ 1 2))

(check '() => ())

(check '(quote a) => (quote a))
(check       ''a  => (quote a))

(check '"abc" => "abc")
(check  "abc" => "abc")

(check '145932 => 145932)
(check  145932 => 145932)

(check '#t => #t)
(check  #t => #t)

; ---- 4.1.3. Procedure calls --------------------------------------------------

(check (+ 3 4) => 7)

(check ((if #f + *) 3 4) => 12)

; ---- 4.1.4. Lambda expressions -----------------------------------------------

(lambda (x) (+ x x)) ; => unspecified

(check ((lambda (x) (+ x x)) 4) => 8)

(define reverse-subtract
  (lambda (x y)
    (- y x))) ; => unspecified

(check (reverse-subtract 7 10) => 3)

(define add4
  (let ((x 4))
    (lambda (y)
      (+ x y))))

(check (add4 6) => 10)

(check ((lambda x x) 3 4 5 6) => (3 4 5 6))

(check
  ((lambda (x y . z) z) 3 4 5 6)
  => (5 6))

; ---- 4.1.5. Conditionals -----------------------------------------------------

(check (if (> 3 2) 'yes 'no) => yes)
(check (if (> 2 3) 'yes 'no) => no)

(check
  (if (> 3 2)
      (- 3 2)
      (+ 3 2))
  => 1)

; ---- 4.1.6. Assignments ------------------------------------------------------

(define x 2) ; => unspecified

(check (+ x 1) => 3)

(set! x 4) ; => unspecified

(check (+ x 1) => 5)


; ===== 4.2. Derived expression types ==========================================


; ==== REPORT ==================================================================

(check-report)

(exit (if (check-passed? check::correct)
          exit-success
          exit-failure))
