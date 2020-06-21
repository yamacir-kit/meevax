(check-set-mode! 'report)

(define syntax-rules-transformer
  (lambda (form rename compare)

    (std::cout "SYNTAX-RULES-TRANSFORMER") (newline)
    (std::cout "FORM IS " form) (newline)

    (let ((count 0)
          )
      (define ellipsis-specified? (identifier? (cadr form)))

      (define ellipsis
        (if ellipsis-specified? (cadr form) (rename '...)))

      (define literals
        (if ellipsis-specified? (car (cddr form)) (cadr form)))

      (define forms
        (if ellipsis-specified? (cdr (cddr form)) (cddr form)))

      (std::cout "; syntax-rules\t; debug") (newline)
      (std::cout "; \t\t; ellipsis-specified? = " ellipsis-specified?) (newline)
      (std::cout "; \t\t; ellipsis = " ellipsis) (newline)
      (std::cout "; \t\t; literals = " literals) (newline)
      (std::cout "; \t\t; forms = " forms) (newline)

      #true
      )
    )
  )

(define-syntax syntax-rules
  (er-macro-transformer
    (lambda (form rename compare)
      (syntax-rules-transformer form rename compare))))

; (define-syntax cond
;   (syntax-rules (else =>)
;     ((cond (else result1 result2 ...))
;      (begin result1 result2 ...))
;     ((cond (test => result))
;      (let ((temp test))
;        (if temp (result temp))))
;     ((cond (test => result) clause1 clause2 ...)
;      (let ((temp test))
;        (if temp
;            (result temp)
;            (cond clause1 clause2 ...))))
;     ((cond (test)) test)
;     ((cond (test) clause1 clause2 ...)
;      (let ((temp test))
;        (if temp
;            temp
;            (cond clause1 clause2 ...))))
;     ((cond (test result1 result2 ...))
;      (if test (begin result1 result2 ...)))
;     ((cond (test result1 result2 ...)
;        clause1 clause2 ...)
;      (if test
;          (begin result1 result2 ...)
;          (cond clause1 clause2 ...)))))

; (check
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
       clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...))))
  ; => #true
  ; )

; ==== REPORT ==================================================================

(check-report)

(exit (if (check-passed? check::correct) exit-success exit-failure))
