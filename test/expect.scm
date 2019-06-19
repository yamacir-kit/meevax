(define passed 0)

(define expect
  (macro (expects expression)
   `(let ((result ,expression))
      (if (equal? result ',expects)
          (begin (set! passed (+ passed 1))
                 result)
          (begin (display "; test          ; expected ") ; TODO SUPPORT TAB
                 (display ',expects)
                 (display " as result of ")
                 (display ',expression)
                 (display ", but got ")
                 (display result)
                 (newline)
                 (emergency-exit))))))



