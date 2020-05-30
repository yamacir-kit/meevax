; ==== SRFI-78 Lightweight Testing =============================================
;
; Document
;   https://srfi.schemers.org/srfi-78/srfi-78.html
;
; Reference Implementation
;   https://srfi.schemers.org/srfi-78/check.scm
;
; ==============================================================================


; ==== Utilities ===============================================================

(define check::write write)

(define exit-success 0)
(define exit-failure 1)


; ==== Mode ====================================================================

(define check::mode #false)

(define check-set-mode!
  (lambda (mode)
    (set! check::mode
          (case mode
            ((off)               0)
            ((summary)           1)
            ((report-incorrect) 10)
            ((report)          100)
            (else
              (error "unrecognized mode" mode))))))

(check-set-mode! 'report)


; ==== State ===================================================================

(define check::correct #false)
(define check::incorrect #false)

(define check-reset!
  (lambda ()
    (set! check::correct 0)
    (set! check::incorrect '())))

(define check::add-correct!
  (lambda ()
    (set! check::correct (+ check::correct 1))))

(define check::add-incorrect!
  (lambda (expression actual-result expected-result)
    (set! check::incorrect
          (cons (list expression actual-result expected-result)
                check::incorrect))))

(check-reset!)


; ==== Reporting ===============================================================

(define check::report-expression
  (lambda (expression)
    (newline)
    (check::write expression)
    (display " => ")))

(define check::report-actual-result
  (lambda (actual-result)
    (check::write actual-result)
    (display " ; ")))

(define check::report-correct
  (lambda (cases)
    (display "correct")
    (if (not (= cases 1))
        (begin (display " (")
               (display cases)
               (display " cases checked)")))
    (newline)))

(define check::report-incorrect
  (lambda (expected-result)
    (display "incorrect. expected result: ")
    (check::write expected-result)
    (newline)))

(define check-report
  (lambda ()
    (if (<= 1 check::mode)
        (begin (newline)
               (display "; Checked ")
               (display check::correct)
               (display " correct, ")
               (display (length check::incorrect))
               (display " incorrect.")
               (if (or (null? check::incorrect)
                       (<= check::mode 1))
                   (newline)
                   (let* ((w (car (reverse check::incorrect)))
                          (expression (car w))
                          (actual-result (cadr w))
                          (expected-result (caddr w)))
                     (newline)
                     (display "; Show the first incorrect expression.")
                     (newline)
                     (check::report-expression expression)
                     (check::report-actual-result actual-result)
                     (check::report-incorrect expected-result)
                     ))))))

(define check-passed?
  (lambda (expected-total-count)
    (and (zero? (length check::incorrect))
         (= check::correct expected-total-count))))


; ==== Simple Checks ===========================================================

(define check::proc
  (lambda (expression thunk compare expected-result)
    (case check::mode
      ((0) #false)
      ((1)
       (let ((actual-result (thunk)))
         (if (compare actual-result expected-result)
             (check::add-correct!)
             (check::add-incorrect! expression actual-result expected-result))))
      ((10)
       (let ((actual-result (thunk)))
         (if (compare actual-result expected-result)
             (check::add-correct!)
             (begin (check::report-expression expression)
                    (check::report-actual-result actual-result)
                    (check::report-incorrect expected-result)
                    (check::add-incorrect! expression actual-result expected-result)))))
      ((100)
       (check::report-expression expression)
       (let ((actual-result (thunk)))
         (check::report-actual-result actual-result)
         (if (compare actual-result expected-result)
             (begin (check::report-correct 1)
                    (check::add-correct!))
             (begin (check::report-incorrect expected-result)
                    (check::add-incorrect! expression
                                           actual-result
                                           expected-result)))))
      (else
        (error "unrecognized check::mode" check::mode))
      (if #false #false))))

(define-syntax (check expression rule expected)
  (cond
    ((and (pair? rule)
          (eq? (car rule) '=>)
          (symbol? (cadr rule)))
     `(,check::proc ',expression (lambda () ,expression) ,(cadr rule) ',expected))
    ((and (symbol? rule)
          (eq? rule '=>))
     `(,check ,expression (=> equal?) ,expected))
    (else
      (if #false #false))))
