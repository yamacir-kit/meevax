(define-library (scheme r4rs)
  (import (meevax inexact)
          (only (meevax number) exact-integer? expt exact inexact ratio? ratio-numerator ratio-denominator)
          (only (meevax port) get-ready? standard-input-port standard-output-port)
          (only (meevax string) string-copy)
          (only (meevax syntax) define-syntax)
          (only (meevax vector) vector-fill!)
          (scheme r4rs essential)
          (srfi 45)
          (srfi 211 explicit-renaming))

  (export quote lambda if set! cond case and or let let* letrec begin do delay
          quasiquote define not boolean? eqv? eq? equal? pair? cons car cdr
          set-car! set-cdr! caar cadr cdar cddr caaar caadr cadar caddr cdaar
          cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar
          cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr null?
          list? list length append reverse list-tail list-ref memq memv member
          assq assv assoc symbol? symbol->string string->symbol number? complex?
          real? rational? integer? exact? inexact? = < > <= >= zero? positive?
          negative? odd? even? max min + * - / abs quotient remainder modulo
          gcd lcm numerator denominator floor ceiling truncate round rationalize
          exp log sin cos tan asin acos atan sqrt expt make-rectangular
          make-polar real-part imag-part magnitude angle
          (rename inexact exact->inexact) (rename exact inexact->exact)
          number->string string->number char? char=? char<? char>? char<=?
          char>=? char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
          char-alphabetic? char-numeric? char-whitespace? char-upper-case?
          char-lower-case? char->integer integer->char char-upcase
          char-downcase string? make-string string string-length string-ref
          string-set! string=? string<? string>? string<=? string>=? string-ci=?
          string-ci<? string-ci>? string-ci<=? string-ci>=? substring
          string-append string->list list->string string-copy string-fill!
          vector? make-vector vector vector-length vector-ref vector-set!
          vector->list list->vector vector-fill! procedure? apply map for-each
          force call-with-current-continuation call-with-input-file
          call-with-output-file input-port? output-port? current-input-port
          current-output-port with-input-from-file with-output-to-file
          open-input-file open-output-file close-input-port close-output-port
          read read-char peek-char eof-object? char-ready? write display newline
          write-char load)

  (begin (define-syntax let*
           (er-macro-transformer
             (lambda (form rename compare)
               (if (null? (cadr form))
                   `(,(rename 'let) () ,@(cddr form))
                   `(,(rename 'let) (,(caadr form))
                                    (,(rename 'let*) ,(cdadr form)
                                                     ,@(cddr form)))))))

         (define-syntax do
           (er-macro-transformer
             (lambda (form rename compare)
               (let ((body `(,(rename 'begin) ,@(cdddr form)
                                              (,(rename 'rec) ,@(map (lambda (x)
                                                                       (if (pair? (cddr x))
                                                                           (caddr x)
                                                                           (car x)))
                                                                     (cadr form))))))
                 `(,(rename 'let) ,(rename 'rec) ,(map (lambda (x)
                                                         (list (car x)
                                                               (cadr x)))
                                                       (cadr form))
                                  ,(if (null? (cdaddr form))
                                       `(,(rename 'let) ((,(rename 'it) ,(caaddr form)))
                                                        (,(rename 'if) ,(rename 'it)
                                                                       ,(rename 'it)
                                                                       ,body))
                                       `(,(rename 'if) ,(caaddr form)
                                                       (,(rename 'begin) ,@(cdaddr form))
                                                       ,body)))))))

         (define (numerator x)
           (cond ((ratio? x) (ratio-numerator x))
                 ((exact? x) x)
                 (else (inexact (numerator (exact x))))))

         (define (denominator x)
           (cond ((ratio? x) (ratio-denominator x))
                 ((exact? x) 1)
                 ((integer? x) 1.0)
                 (else (inexact (denominator (exact x))))))

         (define (rationalize x e) ; from Chibi-Scheme lib/scheme/extras.scm (https://ml.cddddr.org/scheme/msg01498.html)
           (define (sr x y return)
             (let ((fx (floor x))
                   (fy (floor y)))
               (cond ((>= fx x) (return fx 1))
                     ((= fx fy) (sr (/ (- y fy))
                                    (/ (- x fx))
                                    (lambda (n d)
                                      (return (+ d (* fx n)) n))))
                     (else (return (+ fx 1) 1)))))
           (let ((return (if (negative? x)
                             (lambda (num den)
                               (/ (- num) den))
                             /))
                 (x (abs x))
                 (e (abs e)))
             (sr (- x e) (+ x e) return)))

         (define (make-rectangular x y)
           (+ x (* y (sqrt -1))))

         (define (make-polar radius phi)
           (make-rectangular (* radius (cos phi))
                             (* radius (sin phi))))

         (define (real-part z)
           (if (%complex? z) (car z) z))

         (define (imag-part z)
           (if (%complex? z) (cdr z) 0))

         (define (magnitude z)
           (sqrt (+ (square (real-part z))
                    (square (imag-part z)))))

         (define (angle z)
           (atan (imag-part z)
                 (real-part z)))

         (define (list-tail x k)
           (let list-tail ((x x)
                           (k k))
             (if (zero? k) x
                 (list-tail (cdr x)
                            (- k 1)))))

         (define (string-fill! s c . o)
           (let ((start (if (and (pair? o)
                                 (exact-integer? (car o)))
                            (car o)
                            0))
                 (end (if (and (pair? o)
                               (pair? (cdr o))
                               (exact-integer? (cadr o)))
                          (cadr o)
                          (string-length s))))
             (let rec ((k (- end 1)))
               (if (<= start k)
                   (begin (string-set! s k c)
                          (rec (- k 1)))))))

         (define %current-input-port standard-input-port)

         (define (current-input-port) %current-input-port)

         (define %current-output-port standard-output-port)

         (define (current-output-port) %current-output-port)

         (define (with-input-from-file path thunk)
           (let ((previous-input-port (current-input-port)))
             (set! %current-input-port (open-input-file path))
             (thunk)
             (set! %current-input-port previous-input-port)))

         (define (with-output-to-file path thunk)
           (let ((previous-output-port (current-output-port)))
             (set! %current-output-port (open-output-file path))
             (thunk)
             (set! %current-output-port previous-output-port)))

         (define (char-ready? . port)
           (get-ready? (if (pair? port)
                           (car port)
                           (current-input-port))))))
