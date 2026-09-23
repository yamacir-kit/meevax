(import (only (meevax core) call-with-current-continuation!)
        (meevax environment)
        (meevax macro-transformer)
        (scheme base)
        (scheme list)
        (scheme process-context)
        (scheme read)
        (scheme repl)
        (scheme write)
        (srfi 78))

(define (canonicalize object)
  (parameterize ((current-output-port (open-output-string)))
    (write object)
    (read (open-input-string (get-output-string (current-output-port))))))

(define (check-compiler form core-form cps-form code . values)
  (let ((expanded (expand form (interaction-environment))))
    (check (canonicalize expanded) => core-form)
    (let ((converted (convert expanded (interaction-environment))))
      (check (canonicalize converted) => cps-form)
      (let ((generated (generate converted (interaction-environment))))
        (check (canonicalize generated) => code)
        (unless (null? values)
          (check (eval form (interaction-environment)) => (car values)))))))

(check-compiler ; Simple procedure call
  '(+ 1 2 3)

  '(+ 1 2 3)

  '(+ #k 1 2 3)

  '(load-constant 3
    load-constant 2
    load-constant 1
    load-constant #k
    load-absolute +
    tail-call)

  6)

(check-compiler ; Simple procedure call
  '(car '(a b))

  '(car '(a b))

  '(car #k '(a b))

  '(load-constant (a b)
    load-constant #k
    load-absolute car
    tail-call)

  'a)

(check-compiler ; Simple sequence
  '(begin (+ 1 2)
          (+ 3 4)
          (+ 5 6))

  '(begin (+ 1 2)
          (+ 3 4)
          (+ 5 6))

  '(+ (<lambda> $values
        (+ (<lambda> $values
             (+ #k 5 6))
           3
           4))
      1
      2)

  '(load-constant 2
    load-constant 1
    load-closure
    ( load-constant 4
      load-constant 3
      load-closure
      ( load-constant 6
        load-constant 5
        load-constant #k
        load-absolute +
        tail-call)
      load-absolute +
      tail-call)
    load-absolute +
    tail-call)

  '11)

(check-compiler ; Redundant sequence
  '(begin (begin (+ 1 2))
          (begin (+ 3 4))
          (begin (+ 5 6)))

  '(begin (begin (+ 1 2))
          (begin (+ 3 4))
          (begin (+ 5 6)))

  '(+ (<lambda> $values
        (+ (<lambda> $values
             (+ #k 5 6))
           3
           4))
      1
      2)

  '(load-constant 2
    load-constant 1
    load-closure
    ( load-constant 4
      load-constant 3
      load-closure
      ( load-constant 6
        load-constant 5
        load-constant #k
        load-absolute +
        tail-call)
      load-absolute +
      tail-call)
    load-absolute +
    tail-call)

  '11)

(check-compiler ; Simple let
  '(let ((x 42))
     (+ x 1))

  '((<lambda> (x)
      (+ x 1))
    42)

  '((<lambda> ($k x)
      (+ $k x 1))
    #k 42)

  '(load-constant 42
    load-constant #k
    load-closure (
      load-constant 1
      load-relative (0 . 1)
      load-relative (0 . 0)
      load-absolute +
      tail-call)
    tail-call)

  43)

(check-compiler ; Simple let
  '(let ((x 1)
         (y 2)
         (z 3))
     (+ x y z))

  '((<lambda> (x y z)
      (+ x y z))
    1 2 3)

  '((<lambda> ($k x y z)
      (+ $k x y z))
    #k 1 2 3)

  '(load-constant 3
    load-constant 2
    load-constant 1
    load-constant #k
    load-closure (
      load-relative (0 . 3)
      load-relative (0 . 2)
      load-relative (0 . 1)
      load-relative (0 . 0)
      load-absolute +
      tail-call)
    tail-call)

  '6)

(check-compiler ; Shadowing
  '(let ((x 1)
         (y 2)
         (z 3))
     (let ((y 4)
           (z 5))
       (+ x y z)))

  '((<lambda> (x y z)
      ((<lambda> (<y%1> <z%1>)
         (+ x <y%1> <z%1>))
       4 5))
    1 2 3)

  '((<lambda> ($k x y z)
      ((<lambda> ($k <y%1> <z%1>)
         (+ $k x <y%1> <z%1>))
       $k 4 5))
     #k 1 2 3)

  '(load-constant 3
    load-constant 2
    load-constant 1
    load-constant #k
    load-closure (
      load-constant 5
      load-constant 4
      load-relative (0 . 0)
      load-closure (
        load-relative (0 . 2)
        load-relative (0 . 1)
        load-relative (1 . 1)
        load-relative (0 . 0)
        load-absolute +
        tail-call)
      tail-call)
    tail-call)

  '10)

(check-compiler ; Shadowing
  '(let ((x 1)
         (y 2)
         (z 3))
     (let ((y 4)
           (z 5))
       (let ((z 6))
         (+ x y z))))

  '((<lambda> (x y z)
      ((<lambda> (<y%1> <z%1>)
         ((<lambda> (<z%2>)
            (+ x <y%1> <z%2>))
          6))
       4 5))
    1 2 3)

  '((<lambda> ($k x y z)
      ((<lambda> ($k <y%1> <z%1>)
         ((<lambda> ($k <z%2>)
            (+ $k x <y%1> <z%2>))
          $k 6))
       $k 4 5))
    #k 1 2 3)

  '(load-constant 3
    load-constant 2
    load-constant 1
    load-constant #k
    load-closure (
      load-constant 5
      load-constant 4
      load-relative (0 . 0)
      load-closure (
        load-constant 6
        load-relative (0 . 0)
        load-closure (
          load-relative (0 . 1)
          load-relative (1 . 1)
          load-relative (2 . 1)
          load-relative (0 . 0)
          load-absolute +
          tail-call)
        tail-call)
      tail-call)
    tail-call)

  '11)

(check-compiler ; Shadowing
  '((lambda (x y z)
      ((lambda (y z)
         ((lambda (z)
            (+ x y z))
          6))
       4 5))
    1 2 3)

  '((lambda (x y z)
      ((lambda (<y%1> <z%1>)
         ((lambda (<z%2>)
            (+ x <y%1> <z%2>))
          6))
       4 5))
    1 2 3)

  '((<lambda> ($k x y z)
      ((<lambda> ($k <y%1> <z%1>)
         ((<lambda> ($k <z%2>)
            (+ $k x <y%1> <z%2>))
          $k 6))
       $k 4 5))
    #k 1 2 3)

  '(load-constant 3
    load-constant 2
    load-constant 1
    load-constant #k
    load-closure (
      load-constant 5
      load-constant 4
      load-relative (0 . 0)
      load-closure (
        load-constant 6
        load-relative (0 . 0)
        load-closure (
          load-relative (0 . 1)
          load-relative (1 . 1)
          load-relative (2 . 1)
          load-relative (0 . 0)
          load-absolute +
          tail-call)
        tail-call)
      tail-call)
    tail-call)

  '11)

(check-compiler ; Redundant expression
  '(let ()
     (let ()
       (let ()
         42)))

  '((<lambda> ()
      ((<lambda> ()
         ((<lambda> ()
            42))))))

  '((<lambda> ($k)
      ((<lambda> ($k)
         ((<lambda> ($k)
            ($k 42))
          $k))
       $k))
    #k)

  '(load-constant #k
    load-closure
    ( load-relative (0 . 0)
      load-closure
      ( load-relative (0 . 0)
        load-closure
        ( load-constant 42
          load-relative (0 . 0)
          tail-call)
        tail-call)
      tail-call)
    tail-call)

  '42)

(check-compiler ; Simple letrec
  '(letrec ((a 1)
            (b 2))
     (+ a b))

  '(letrec ((a 1)
            (b 2))
     (+ a b))

  '((<lambda> ($k a b)
      ((<lambda> ($k $temporary $temporary)
         (<begin> (<set!> a $temporary)
                  ((<lambda> $values
                     (<begin> (<set!> b $temporary)
                              ((<lambda> $values
                                 (+ $k a b))))))))
       $k 1 2))
    #k)

  '(load-constant #;unspecified
    load-constant #;unspecified
    load-constant #k
    load-closure
    ( load-constant 2
      load-constant 1
      load-relative (0 . 0)
      load-closure
      ( load-relative (0 . 1)
        store-relative (1 . 1)
        drop
        load-constant #;unspecified
        load-closure
        ( load-relative (1 . 2)
          store-relative (2 . 2)
          drop
          load-constant #;unspecified
          load-closure
          ( load-relative (3 . 2)
            load-relative (3 . 1)
            load-relative (2 . 0)
            load-absolute +
            tail-call)
          tail-call)
        tail-call)
      tail-call)
    tail-call)

  '3)

(check-compiler ; Conditional expression
  '(cond ((> 3 2) 'greater)
         ((< 3 2) 'less))

  '(<if> (> 3 2)
         (<begin> 'greater)
         (<if> (< 3 2)
               (<begin> 'less)))

  '(> (<lambda> ($value)
        (<if> $value
              (#k 'greater)
              (< (<lambda> ($value)
                   (<if> $value (#k 'less) (#k)))
                 3
                 2)))
      3
      2)

  '(load-constant 2
    load-constant 3
    load-closure
    ( load-relative (0 . 0)
      select
      ( load-constant greater
        load-constant #k
        tail-call)
      ( load-constant 2
        load-constant 3
        load-closure
        ( load-relative (0 . 0)
          select
          ( load-constant less
            load-constant #k
            tail-call)
          ( load-constant #;unspecified
            load-constant #k
            tail-call))
        load-absolute <
        tail-call))
    load-absolute >
    tail-call)

  'greater)

(check-compiler ; Internal definitions
  '(let ()
     (define x 1)
     (define y (+ x 1))
     (+ x y))

  '((<lambda> ()
      ((<lambda> (x y)
         (<set!> x 1)
         (<set!> y (+ x 1))
         (+ x y))
       ()
       ())))

  '((<lambda> ($k)
      ((<lambda> ($k x y)
         (<begin> (<set!> x 1)
                  ((<lambda> $values
                     (+ (<lambda> ($value)
                          (<begin> (<set!> y $value)
                                   ((<lambda> $values (+ $k x y)))))
                        x
                        1)))))
       $k
       ()
       ()))
    #k)

  '(load-constant #k
    load-closure
    ( load-constant ()
      load-constant ()
      load-relative (0 . 0)
      load-closure
      ( load-constant 1
        store-relative (0 . 1)
        drop
        load-constant #;unspecified
        load-closure
        ( load-constant 1
          load-relative (1 . 1)
          load-closure
          ( load-relative (0 . 0)
            store-relative (2 . 2)
            drop
            load-constant #;unspecified
            load-closure
            ( load-relative (3 . 2)
              load-relative (3 . 1)
              load-relative (3 . 0)
              load-absolute +
              tail-call)
            tail-call)
          load-absolute +
          tail-call)
        tail-call)
      tail-call)
    tail-call)

  3)

(check-compiler ; Internal definitions
  '(let ((x 1)
         (y 2))
     (define x 10)
     (define y 20)
     (+ x y))

  '((<lambda> (x y)
      ((<lambda> (<x%1> <y%1>)
         (<set!> <x%1> 10)
         (<set!> <y%1> 20)
         (+ <x%1> <y%1>))
       ()
       ()))
    1
    2)

  '((<lambda> ($k x y)
      ((<lambda> ($k <x%1> <y%1>)
         (<begin> (<set!> <x%1> 10)
                  ((<lambda> $values
                     (<begin> (<set!> <y%1> 20)
                              ((<lambda> $values (+ $k <x%1> <y%1>))))))))
       $k
       ()
       ()))
    #k
    1
    2)

  '(load-constant 2
    load-constant 1
    load-constant #k
    load-closure
    ( load-constant ()
      load-constant ()
      load-relative (0 . 0)
      load-closure
      ( load-constant 10
        store-relative (0 . 1)
        drop
        load-constant
        load-closure
        ( load-constant 20
          store-relative (1 . 2)
          drop
          load-constant #;unspecified
          load-closure
          ( load-relative (2 . 2)
            load-relative (2 . 1)
            load-relative (2 . 0)
            load-absolute +
            tail-call)
          tail-call)
        tail-call)
      tail-call)
    tail-call)

  '30)

(check-compiler ; Internal procedure definitions
  '(let ((x 5))
     (define foo (lambda (y) (bar x y)))
     (define bar (lambda (a b) (+ (* a b) a)))
     (foo (+ x 3)))

  '((<lambda> (x)
      ((<lambda> (foo bar)
         (<set!> foo (lambda (y) (bar x y)))
         (<set!> bar (lambda (a b) (+ (* a b) a)))
         (foo (+ x 3)))
       ()
       ()))
    5)

  '((<lambda> ($k x)
      ((<lambda> ($k foo bar)
         (<begin> (<set!> foo (<lambda> ($k y)
                                (bar $k x y)))
                  ((<lambda> $values
                     (<begin> (<set!> bar (<lambda> ($k a b)
                                            (* (<lambda> ($value)
                                                 (+ $k $value a))
                                               a
                                               b)))
                              ((<lambda> $values
                                 (+ (<lambda> ($value)
                                      (foo $k $value))
                                    x
                                    3))))))))
       $k
       ()
       ()))
    #k
    5)

  '(load-constant 5
    load-constant #k
    load-closure
    ( load-constant ()
      load-constant ()
      load-relative (0 . 0)
      load-closure
      ( load-closure
        ( load-relative (0 . 1)
          load-relative (2 . 1)
          load-relative (0 . 0)
          load-relative (1 . 2)
          tail-call)
        store-relative (0 . 1)
        drop
        load-constant #;unspecified
        load-closure
        ( load-closure
          ( load-relative (0 . 2)
            load-relative (0 . 1)
            load-closure
            ( load-relative (1 . 1)
              load-relative (0 . 0)
              load-relative (1 . 0)
              load-absolute +
              tail-call)
            load-absolute *
            tail-call)
          store-relative (1 . 2)
          drop
          load-constant
          load-closure
          ( load-constant 3
            load-relative (3 . 1)
            load-closure
            ( load-relative (0 . 0)
              load-relative (3 . 0)
              load-relative (3 . 1)
              tail-call)
            load-absolute +
            tail-call)
          tail-call)
        tail-call)
      tail-call)
    tail-call)

  '45)

(check-compiler ; Internal procedure definitions
  '(let ((f 1)
         (g 2))
     (define (f x) (+ x 10))
     (define (g x) (+ x 20))
     (g (f 3)))

  '((<lambda> (f g)
      ((<lambda> (<f%1> <g%1>)
         (<set!> <f%1> (<lambda> (x) (+ x 10)))
         (<set!> <g%1> (<lambda> (x) (+ x 20)))
         (<g%1> (<f%1> 3)))
       ()
       ()))
    1
    2)

  '((<lambda> ($k f g)
      ((<lambda> ($k <f%1> <g%1>)
         (<begin> (<set!> <f%1> (<lambda> ($k x)
                                  (+ $k x 10)))
                  ((<lambda> $values
                     (<begin> (<set!> <g%1> (<lambda> ($k x)
                                              (+ $k x 20)))
                              ((<lambda> $values
                                 (<f%1> (<lambda> ($value)
                                          (<g%1> $k $value))
                                        3))))))))
       $k
       ()
       ()))
    #k
    1
    2)

  '(load-constant 2
    load-constant 1
    load-constant #k
    load-closure
    ( load-constant ()
      load-constant ()
      load-relative (0 . 0)
      load-closure
      ( load-closure
        ( load-constant 10
          load-relative (0 . 1)
          load-relative (0 . 0)
          load-absolute +
          tail-call)
        store-relative (0 . 1)
        drop
        load-constant
        load-closure
        ( load-closure
          ( load-constant 20
            load-relative (0 . 1)
            load-relative (0 . 0)
            load-absolute +
            tail-call)
          store-relative (1 . 2)
          drop
          load-constant
          load-closure
          ( load-constant 3
            load-closure
            ( load-relative (0 . 0)
              load-relative (3 . 0)
              load-relative (3 . 2)
              tail-call)
            load-relative (2 . 1)
            tail-call)
          tail-call)
        tail-call)
      tail-call)
    tail-call)

  '33)

(check-compiler ; Complex internal procedure definitions
  '(define (f x)
     (define (g1 x)
       (define (h x)
         (+ x 10))
       (cond ((< 0 x)
              (h x))
             (else x)))
     (define (g2 x)
       (+ x 1))
     (g1 (g2 x)))

  '(define f
     (<lambda> (x)
       ((<lambda> (g1 g2)
          (<set!> g1 (<lambda> (<x%2>)
                       ((<lambda> (h)
                          (<set!> h (<lambda> (<x%4>)
                                      (+ <x%4> 10)))
                          (<if> (< 0 <x%2>)
                                (<begin> (h <x%2>))
                                (<begin> <x%2>)))
                        ())))
          (<set!> g2 (<lambda> (<x%2>)
                       (+ <x%2> 1)))
          (g1 (g2 x)))
        ()
        ())))

  '(<begin> (<set!> f (<lambda> ($k x)
                        ((<lambda> ($k g1 g2)
                           (<begin> (<set!> g1 (<lambda> ($k <x%2>)
                                                 ((<lambda> ($k h)
                                                    (<begin> (<set!> h (<lambda> ($k <x%4>)
                                                                         (+ $k <x%4> 10)))
                                                             ((<lambda> $values
                                                                (< (<lambda> ($value)
                                                                     (<if> $value
                                                                           (h $k <x%2>)
                                                                           ($k <x%2>)))
                                                                   0
                                                                   <x%2>)))))
                                                  $k
                                                  ())))
                                    ((<lambda> $values
                                       (<begin> (<set!> g2 (<lambda> ($k <x%2>)
                                                             (+ $k <x%2> 1)))
                                                ((<lambda> $values
                                                   (g2 (<lambda> ($value)
                                                         (g1 $k $value))
                                                       x))))))))
                         $k
                         ()
                         ())))
            (#k))

  '(load-closure
    ( load-constant ()
      load-constant ()
      load-relative (0 . 0)
      load-closure
      ( load-closure
        ( load-constant ()
          load-relative (0 . 0)
          load-closure
          ( load-closure
            ( load-constant 10
              load-relative (0 . 1)
              load-relative (0 . 0)
              load-absolute +
              tail-call)
            store-relative (0 . 1)
            drop
            load-constant #;unspecified
            load-closure
            ( load-relative (2 . 1)
              load-constant 0
              load-closure
              ( load-relative (0 . 0)
                select
                ( load-relative (3 . 1)
                  load-relative (2 . 0)
                  load-relative (2 . 1)
                  tail-call)
                ( load-relative (3 . 1)
                  load-relative (2 . 0)
                  tail-call))
              load-absolute <
              tail-call)
            tail-call)
          tail-call)
        store-relative (0 . 1)
        drop
        load-constant #;unspecified
        load-closure
        ( load-closure
          ( load-constant 1
            load-relative (0 . 1)
            load-relative (0 . 0)
            load-absolute +
            tail-call)
          store-relative (1 . 2)
          drop
          load-constant #;unspecified
          load-closure
          ( load-relative (3 . 1)
            load-closure
            ( load-relative (0 . 0)
              load-relative (3 . 0)
              load-relative (3 . 1)
              tail-call)
            load-relative (2 . 2)
            tail-call)
          tail-call)
        tail-call)
      tail-call)
    store-absolute f
    drop
    load-constant #;unspecified
    load-constant #k
    tail-call)

  (if #f #f))

(check-compiler
  '(f 0)

  '(f 0)

  '(f #k 0)

  '(load-constant 0
    load-constant #k
    load-absolute f
    tail-call)

  11)

(check-compiler ; Call-with-current-continuation
  '(call-with-current-continuation!
     (lambda (return)
       (return)))

  '(call-with-current-continuation!
     (lambda (return)
       (return)))

  '((<lambda> ($k return)
      (return $k))
    #k
    (<lambda> ($_ . $values)
      (#k . $values)))

  '(load-closure
    ( load-variadic (0 . 1)
      list-values
      load-constant #k
      tail-call)
    load-constant #k
    load-closure
    ( load-relative (0 . 0)
      load-relative (0 . 1)
      tail-call)
    tail-call))

(check-compiler ; Call-with-current-continuation
  '(call-with-current-continuation
     (lambda (return)
       (return)))

  '(call-with-current-continuation
     (lambda (return)
       (return)))

  '(call-with-current-continuation
     #k
     (<lambda> ($k return)
       (return $k)))

  '(load-closure
    ( load-relative (0 . 0)
      load-relative (0 . 1)
      tail-call)
    load-constant #k
    load-absolute call-with-current-continuation
    tail-call))

(check-compiler ; Multiple values (call-with-values)
  '(call-with-values values list)

  '(call-with-values values list)

  '(call-with-values #k values list)

  '(load-absolute list
    load-absolute values
    load-constant #k
    load-absolute call-with-values
    tail-call)

  '())

(check-compiler ; Multiple values (call-with-values)
  '(call-with-values (lambda () (values))
                     list)

  '(call-with-values (lambda () (values))
                     list)

  '(call-with-values #k
                     (<lambda> ($k)
                       (values $k))
                     list)

  '(load-absolute list
    load-closure
    ( load-relative (0 . 0)
      load-absolute values
      tail-call)
    load-constant #k
    load-absolute call-with-values
    tail-call)

  '())

(check-compiler ; Multiple values (call-with-values)
  '(call-with-values (lambda ()
                       (values 1))
                     list)

  '(call-with-values (lambda ()
                       (values 1))
                     list)

  '(call-with-values #k
                     (<lambda> ($k)
                       (values $k 1))
                     list)

  '(load-absolute list
    load-closure
    ( load-constant 1
      load-relative (0 . 0)
      load-absolute values
      tail-call)
    load-constant #k
    load-absolute call-with-values
    tail-call)

  '(1))

(check-compiler ; Multiple values (call-with-values)
  '(call-with-values (lambda ()
                       (values 1 2))
                     list)

  '(call-with-values (lambda ()
                       (values 1 2))
                     list)

  '(call-with-values #k
                     (<lambda> ($k)
                       (values $k 1 2))
                     list)

  '(load-absolute list
    load-closure
    ( load-constant 2
      load-constant 1
      load-relative (0 . 0)
      load-absolute values
      tail-call)
    load-constant #k
    load-absolute call-with-values
    tail-call)

  '(1 2))

(check-compiler ; Multiple values (call-with-values)
  '(call-with-values (lambda () 42) list)

  '(call-with-values (lambda () 42) list)

  '(call-with-values #k
                     (<lambda> ($k)
                       ($k 42))
                     list)

  '(load-absolute list
    load-closure
    ( load-constant 42
      load-relative (0 . 0)
      tail-call)
    load-constant #k
    load-absolute call-with-values
    tail-call)

  '(42))

(check-compiler ; Multiple values (call-with-values)
  '(call-with-values (lambda ()
                       (call-with-values (lambda ()
                                           (values 1 2))
                                         values))
                     list)

  '(call-with-values (lambda ()
                       (call-with-values (lambda ()
                                           (values 1 2))
                                         values))
                     list)

  '(call-with-values #k
                     (<lambda> ($k)
                       (call-with-values $k
                                         (<lambda> ($k)
                                           (values $k 1 2))
                                         values))
                     list)

  '(load-absolute list
    load-closure
    ( load-absolute values
      load-closure
      ( load-constant 2
        load-constant 1
        load-relative (0 . 0)
        load-absolute values
        tail-call)
      load-relative (0 . 0)
      load-absolute call-with-values
      tail-call)
    load-constant #k
    load-absolute call-with-values
    tail-call)

  '(1 2))

(check-compiler ; Multiple values (call-with-values)
  '(call-with-values (lambda ()
                       (call-with-current-continuation
                         (lambda (k)
                           (k))))
                     list)

  '(call-with-values (lambda ()
                       (call-with-current-continuation
                         (lambda (k)
                           (k))))
                     list)

  '(call-with-values #k
                     (<lambda> ($k)
                       (call-with-current-continuation
                         $k
                         (<lambda> ($k k)
                           (k $k))))
                     list)

  '(load-absolute list
    load-closure
    ( load-closure
      ( load-relative (0 . 0)
        load-relative (0 . 1)
        tail-call)
      load-relative (0 . 0)
      load-absolute call-with-current-continuation
      tail-call)
    load-constant #k
    load-absolute call-with-values
    tail-call)

  '())

(check-compiler ; Multiple values (call-with-values)
  '(call-with-values (lambda ()
                       (call-with-current-continuation
                         (lambda (k)
                           (k 1))))
                     list)

  '(call-with-values (lambda ()
                       (call-with-current-continuation
                         (lambda (k)
                           (k 1))))
                     list)

  '(call-with-values #k
                     (<lambda> ($k)
                       (call-with-current-continuation
                         $k
                         (<lambda> ($k k)
                           (k $k 1))))
                     list)

  '(load-absolute list
    load-closure
    ( load-closure
      ( load-constant 1
        load-relative (0 . 0)
        load-relative (0 . 1)
        tail-call)
      load-relative (0 . 0)
      load-absolute call-with-current-continuation
      tail-call)
    load-constant #k
    load-absolute call-with-values
    tail-call)

  '(1))

(check-compiler ; Multiple values (call-with-values)
  '(call-with-values (lambda ()
                       (call-with-current-continuation
                         (lambda (k)
                           (k 1 2))))
                     list)

  '(call-with-values (lambda ()
                       (call-with-current-continuation
                         (lambda (k)
                           (k 1 2))))
                     list)

  '(call-with-values #k
                     (<lambda> ($k)
                       (call-with-current-continuation
                         $k
                         (<lambda> ($k k)
                           (k $k 1 2))))
                     list)

  '(load-absolute list
    load-closure
    ( load-closure
      ( load-constant 2
        load-constant 1
        load-relative (0 . 0)
        load-relative (0 . 1)
        tail-call)
      load-relative (0 . 0)
      load-absolute call-with-current-continuation
      tail-call)
    load-constant #k
    load-absolute call-with-values
    tail-call)

  '(1 2))

(check-compiler ; Multiple values (internal define-values)
  '(let ()
     (define-values (x y)
       (values 1 2))
     (+ x y))

  '((<lambda> ()
      ((<lambda> (x y)
         (<set!> x (<call-with-values> (<lambda> ()
                                         (values 1 2))
                                       <list>))
         (<set!> y ((<lambda> (<x>)
                      (<set!> x (<car> x))
                      <x>)
                    (<cadr> x)))
         (+ x y))
       ()
       ())))

  '((<lambda> ($k)
      ((<lambda> ($k x y)
         (<call-with-values> (<lambda> ($value)
                               (<begin> (<set!> x $value)
                                        ((<lambda> $values
                                           (<cadr> (<lambda> ($value)
                                                     ((<lambda> ($k <x>)
                                                        (<car> (<lambda> ($value)
                                                                 (<begin> (<set!> x $value)
                                                                          ((<lambda> $values
                                                                             ($k <x>)))))
                                                               x))
                                                      (<lambda> ($value)
                                                        (<begin> (<set!> y $value)
                                                                 ((<lambda> $values
                                                                    (+ $k x y)))))
                                                      $value))
                                                   x)))))
                             (<lambda> ($k)
                               (values $k 1 2))
                             <list>))
       $k
       ()
       ()))
    #k)

  '(load-constant #k
    load-closure
    ( load-constant ()
      load-constant ()
      load-relative (0 . 0)
      load-closure
      ( load-absolute list
        load-closure
        ( load-constant 2
          load-constant 1
          load-relative (0 . 0)
          load-absolute values
          tail-call)
        load-closure
        ( load-relative (0 . 0)
          store-relative (1 . 1)
          drop
          load-constant #;unspecified
          load-closure
          ( load-relative (2 . 1)
            load-closure
            ( load-relative (0 . 0)
              load-closure
              ( load-relative (0 . 0)
                store-relative (4 . 2)
                drop
                load-constant #;unspecified
                load-closure
                ( load-relative (5 . 2)
                  load-relative (5 . 1)
                  load-relative (5 . 0)
                  load-absolute +
                  tail-call)
                tail-call)
              load-closure
              ( load-relative (4 . 1)
                load-closure
                ( load-relative (0 . 0)
                  store-relative (5 . 1)
                  drop
                  load-constant #;unspecified
                  load-closure
                  ( load-relative (2 . 1)
                    load-relative (2 . 0)
                    tail-call)
                  tail-call)
                load-absolute car
                tail-call)
              tail-call)
            load-absolute cadr
            tail-call)
          tail-call)
        load-absolute call-with-values
        tail-call)
      tail-call)
    tail-call)

  '3)

(check-compiler ; Multiple values (let*-values)
  '(let ((a 'A)
         (b 'B)
         (x 'X)
         (y 'Y))
     (let*-values (((a b) (values x y))
                   ((x y) (values a b)))
       (list a b x y)))

  '((<lambda> (a b x y)
      (<call-with-values> (<lambda> ()
                            (values x y))
                          (<lambda> (<x> <x>)
                            ((<lambda> (<a%2> <b%2>)
                               (<call-with-values> (<lambda> ()
                                                     (values <a%2> <b%2>))
                                                   (<lambda> (<x> <x>)
                                                     ((<lambda> (<x%4> <y%4>)
                                                        ((<lambda> ()
                                                           (list <a%2> <b%2> <x%4> <y%4>))))
                                                      <x> <x>))))
                             <x> <x>))))
    'A 'B 'X 'Y)

  '((<lambda> ($k a b x y)
      (<call-with-values> $k
                          (<lambda> ($k)
                            (values $k x y))
                          (<lambda> ($k <x> <x>)
                            ((<lambda> ($k <a%2> <b%2>)
                               (<call-with-values> $k
                                                   (<lambda> ($k)
                                                     (values $k <a%2> <b%2>))
                                                   (<lambda> ($k <x> <x>)
                                                     ((<lambda> ($k <x%4> <y%4>)
                                                        ((<lambda> ($k)
                                                           (list $k <a%2> <b%2> <x%4> <y%4>))
                                                         $k))
                                                      $k <x> <x>))))
                             $k <x> <x>))))
    #k 'A 'B 'X 'Y)

  '(load-constant Y
    load-constant X
    load-constant B
    load-constant A
    load-constant #k
    load-closure
    ( load-closure
      ( load-relative (0 . 2)
        load-relative (0 . 1)
        load-relative (0 . 0)
        load-closure
        ( load-closure
          ( load-relative (0 . 2)
            load-relative (0 . 1)
            load-relative (0 . 0)
            load-closure
            ( load-relative (0 . 0)
              load-closure
              ( load-relative (1 . 2)
                load-relative (1 . 1)
                load-relative (3 . 2)
                load-relative (3 . 1)
                load-relative (0 . 0)
                load-absolute list
                tail-call)
              tail-call)
            tail-call)
          load-closure
          ( load-relative (1 . 2)
            load-relative (1 . 1)
            load-relative (0 . 0)
            load-absolute values
            tail-call)
          load-relative (0 . 0)
          load-absolute call-with-values
          tail-call)
        tail-call)
      load-closure
      ( load-relative (1 . 4)
        load-relative (1 . 3)
        load-relative (0 . 0)
        load-absolute values
        tail-call)
      load-relative (0 . 0)
      load-absolute call-with-values
      tail-call)
    tail-call)

  '(X Y X Y))

(check-compiler ; Local syntax definition (let-syntax)
  '(let ((x 'outer))
     (let-syntax ((m (syntax-rules ()
                       ((m) x))))
       (let ((x 'inner))
         (m))))

  '((<lambda> (x)
      ((<lambda> (m)
         ((<lambda> (<x%2>)
            <<x>>)
          'inner))))
    'outer)

  '((<lambda> ($k x)
      ((<lambda> ($k m)
         ((<lambda> ($k <x%2>)
            ($k <<x>>))
          $k
          'inner))
       $k))
    #k
    'outer)

  '(load-constant outer
    load-constant #k
    load-closure
    ( load-relative (0 . 0)
      load-closure
      ( load-constant inner
        load-relative (0 . 0)
        load-closure
        ( load-relative (2 . 1)
          load-relative (0 . 0)
          tail-call)
        tail-call)
      tail-call)
    tail-call)

  'outer)

(check-compiler ; Local syntax definition (letrec-syntax)
  '(let ((x 'outer))
     (letrec-syntax ((m (syntax-rules ()
                          ((m) x))))
       (let ((x 'inner))
         (m))))

  '((<lambda> (x)
      ((<lambda> (m)
         ((<lambda> (<x%2>)
            <<x>>)
          'inner))))
    'outer)

  '((<lambda> ($k x)
      ((<lambda> ($k m)
         ((<lambda> ($k <x%2>)
            ($k <<x>>))
          $k
          'inner))
       $k))
    #k
    'outer)

  '(load-constant outer
    load-constant #k
    load-closure
    ( load-relative (0 . 0)
      load-closure
      ( load-constant inner
        load-relative (0 . 0)
        load-closure
        ( load-relative (2 . 1)
          load-relative (0 . 0)
          tail-call)
        tail-call)
      tail-call)
    tail-call)

  'outer)

(define-syntax sc-swap!
  (sc-macro-transformer
    (lambda (form on-use)
      (let ((a (make-syntactic-closure on-use '() (cadr form)))
            (b (make-syntactic-closure on-use '() (caddr form))))
        `(let ((x ,a))
           (set! ,a ,b)
           (set! ,b x))))))

(check-compiler
  '(let ((x 1)
         (y 2))
     (sc-swap! x y))

  '((<lambda> (x y)
      ((<lambda> (<x%1>)
         (<set!%-1> <x> <y>)
         (<set!%-1> <y> <x%1>))
       <x>))
    1
    2)

  '((<lambda> ($k x y)
      ((<lambda> ($k <x%1>)
         (<begin> (<set!%-1> <x> <y>)
                  ((<lambda> $values
                     (<begin> (<set!%-1> <y> <x%1>)
                              ($k))))))
       $k
       <x>))
    #k
    1
    2)

  '(load-constant 2
    load-constant 1
    load-constant #k
    load-closure
    ( load-relative (0 . 1)
      load-relative (0 . 0)
      load-closure
      ( load-relative (1 . 2)
        store-relative (1 . 1)
        drop
        load-constant #;unspecified
        load-closure
        ( load-relative (1 . 1)
          store-relative (2 . 2)
          drop
          load-constant #;unspecified
          load-relative (1 . 0)
          tail-call)
        tail-call)
      tail-call)
    tail-call)

  (if #f #f))

(check-compiler
  '(let ((x 1)
         (y 2))
     (let ((a 'A)
           (b 'B)
           (let 'LET)
           (set! 'SET!))
       (sc-swap! x y)))

  '((<lambda> (x y)
      ((<lambda> (a b let set!)
         ((<lambda> (<x%2>)
            (<set!%-1> <x> <y>)
            (<set!%-1> <y> <x%2>))
          <x>))
       'A 'B 'LET 'SET!))
    1 2)

  '((<lambda> ($k x y)
      ((<lambda> ($k a b let set!)
         ((<lambda> ($k <x%2>)
            (<begin> (<set!%-1> <x> <y>)
                     ((<lambda> $values
                        (<begin> (<set!%-1> <y> <x%2>)
                                 ($k))))))
          $k
          <x>))
       $k 'A 'B 'LET 'SET!))
     #k 1 2)

  '(load-constant 2
    load-constant 1
    load-constant #k
    load-closure
    ( load-constant SET!
      load-constant LET
      load-constant B
      load-constant A
      load-relative (0 . 0)
      load-closure
      ( load-relative (1 . 1)
        load-relative (0 . 0)
        load-closure
        ( load-relative (2 . 2)
          store-relative (2 . 1)
          drop
          load-constant #;unspecified
          load-closure
          ( load-relative (1 . 1)
            store-relative (3 . 2)
            drop
            load-constant #;unspecified
            load-relative (1 . 0)
            tail-call)
          tail-call)
        tail-call)
      tail-call)
    tail-call)

  (if #f #f))

(check-compiler
  '(let ((x 1)
         (y 2))
     (let-syntax ((local-sc-swap!
                    (sc-macro-transformer
                      (lambda (form on-use)
                        (let ((a (make-syntactic-closure on-use '() (cadr form)))
                              (b (make-syntactic-closure on-use '() (caddr form))))
                          `(let ((x ,a))
                             (set! ,a ,b)
                             (set! ,b x)))))))
       (local-sc-swap! x y)))

  '((<lambda> (x y)
      ((<lambda> (local-sc-swap!)
         ((<lambda> (<x%2>)
            (<set!%-1> <x> <y>)
            (<set!%-1> <y> <x%2>))
          <x>))))
    1 2)

  '((<lambda> ($k x y)
      ((<lambda> ($k local-sc-swap!)
         ((<lambda> ($k <x%2>)
            (<begin> (<set!%-1> <x> <y>)
                     ((<lambda> $values
                        (<begin> (<set!%-1> <y> <x%2>)
                                 ($k))))))
          $k <x>))
       $k))
    #k 1 2)

  '(load-constant 2
    load-constant 1
    load-constant #k
    load-closure
    ( load-relative (0 . 0)
      load-closure
      ( load-relative (1 . 1)
        load-relative (0 . 0)
        load-closure
        ( load-relative (2 . 2)
          store-relative (2 . 1)
          drop
          load-constant #;unspecified
          load-closure
          ( load-relative (1 . 1)
            store-relative (3 . 2)
            drop
            load-constant #;unspecified
            load-relative (1 . 0)
            tail-call)
          tail-call)
        tail-call)
      tail-call)
    tail-call)

  (if #f #f))

(define-syntax rsc-swap!
  (rsc-macro-transformer
    (lambda (form environment)
      (let ((a (cadr form))
            (b (caddr form))
            (x (make-syntactic-closure environment '() 'x))
            (let (make-syntactic-closure environment '() 'let))
            (set! (make-syntactic-closure environment '() 'set!)))
        `(,let ((,x ,a))
           (,set! ,a ,b)
           (,set! ,b ,x))))))

(check-compiler
  '(let ((x 1)
         (y 2))
     (rsc-swap! x y))

  '((<lambda> (x y)
      ((<lambda> (<x>)
         (<set!> x y)
         (<set!> y <x>))
       x))
    1 2)

  '((<lambda> ($k x y)
      ((<lambda> ($k <x>)
         (<begin> (<set!> x y)
                  ((<lambda> $values
                     (<begin> (<set!> y <x>)
                              ($k))))))
       $k x))
    #k 1 2)

  '(load-constant 2
    load-constant 1
    load-constant #k
    load-closure
    ( load-relative (0 . 1)
      load-relative (0 . 0)
      load-closure
      ( load-relative (1 . 2)
        store-relative (1 . 1)
        drop
        load-constant #;unspecified
        load-closure
        ( load-relative (1 . 1)
          store-relative (2 . 2)
          drop
          load-constant #;unspecified
          load-relative (1 . 0)
          tail-call)
        tail-call)
      tail-call)
    tail-call)

  (if #f #f))

(check-compiler
  '(let ((x 1)
         (y 2))
     (let-syntax ((local-rsc-swap!
                    (rsc-macro-transformer
                      (lambda (form environment)
                        (let ((a (cadr form))
                              (b (caddr form))
                              (x (make-syntactic-closure environment '() 'x))
                              (let (make-syntactic-closure environment '() 'let))
                              (set! (make-syntactic-closure environment '() 'set!)))
                          `(,let ((,x ,a))
                             (,set! ,a ,b)
                             (,set! ,b ,x)))))))
       (local-rsc-swap! x y)))

  '((<lambda> (x y)
      ((<lambda> (local-rsc-swap!)
         ((<lambda> (<x>)
            (<set!> x y)
            (<set!> y <x>))
          x))))
    1 2)

  '((<lambda> ($k x y)
      ((<lambda> ($k local-rsc-swap!)
         ((<lambda> ($k <x>)
            (<begin> (<set!> x y)
                     ((<lambda> $values
                        (<begin> (<set!> y <x>)
                                 ($k))))))
          $k x))
       $k))
    #k 1 2)

  '(load-constant 2
    load-constant 1
    load-constant #k
    load-closure
    ( load-relative (0 . 0)
      load-closure
      ( load-relative (1 . 1)
        load-relative (0 . 0)
        load-closure
        ( load-relative (2 . 2)
          store-relative (2 . 1)
          drop
          load-constant #;unspecified
          load-closure
          ( load-relative (1 . 1)
            store-relative (3 . 2)
            drop
            load-constant #;unspecified
            load-relative (1 . 0)
            tail-call)
          tail-call)
        tail-call)
      tail-call)
    tail-call)

  (if #f #f))

(define-syntax er-swap!
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((a (cadr form))
            (b (caddr form)))
        `(,(rename 'let) ((,(rename 'x) ,a))
                         (,(rename 'set!) ,a ,b)
                         (,(rename 'set!) ,b ,(rename 'x)))))))

(check-compiler
  '(let ((x 1)
         (y 2))
     (er-swap! x y))

  '((<lambda> (x y)
      ((<lambda> (<x>)
         (<set!> x y)
         (<set!> y <x>))
       x))
    1 2)

  '((<lambda> ($k x y)
      ((<lambda> ($k <x>)
         (<begin> (<set!> x y)
                  ((<lambda> $values
                     (<begin> (<set!> y <x>)
                              ($k))))))
       $k x))
    #k 1 2)

  '(load-constant 2
    load-constant 1
    load-constant #k
    load-closure
    ( load-relative (0 . 1)
      load-relative (0 . 0)
      load-closure
      ( load-relative (1 . 2)
        store-relative (1 . 1)
        drop
        load-constant #;unspecified
        load-closure
        ( load-relative (1 . 1)
          store-relative (2 . 2)
          drop
          load-constant #;unspecified
          load-relative (1 . 0)
          tail-call)
        tail-call)
      tail-call)
    tail-call)

  (if #f #f))

(check-compiler
  '(let ((x 1)
         (y 2))
     (let-syntax ((local-er-swap!
                    (er-macro-transformer
                      (lambda (form rename compare)
                        (let ((a (cadr form))
                              (b (caddr form)))
                          `(,(rename 'let) ((,(rename 'x) ,a))
                             (,(rename 'set!) ,a ,b)
                             (,(rename 'set!) ,b ,(rename 'x))))))))
       (local-er-swap! x y)))

  '((<lambda> (x y)
      ((<lambda> (local-er-swap!)
         ((<lambda> (<x>)
            (<set!> x y)
            (<set!> y <x>))
          x))))
    1 2)

  '((<lambda> ($k x y)
      ((<lambda> ($k local-er-swap!)
         ((<lambda> ($k <x>)
            (<begin> (<set!> x y)
                     ((<lambda> $values
                        (<begin> (<set!> y <x>)
                                 ($k))))))
          $k x))
       $k))
    #k 1 2)

  '(load-constant 2
    load-constant 1
    load-constant #k
    load-closure
    ( load-relative (0 . 0)
      load-closure
      ( load-relative (1 . 1)
        load-relative (0 . 0)
        load-closure
        ( load-relative (2 . 2)
          store-relative (2 . 1)
          drop
          load-constant #;unspecified
          load-closure
          ( load-relative (1 . 1)
            store-relative (3 . 2)
            drop
            load-constant #;unspecified
            load-relative (1 . 0)
            tail-call)
          tail-call)
        tail-call)
      tail-call)
    tail-call)

  (if #f #f))

(define-syntax swap!
  (syntax-rules ()
    ((swap! a b)
     (let ((x a))
       (set! a b)
       (set! b x)))))

(check-compiler
  '(let ((x 1)
         (y 2))
     (swap! x y))

  '((<lambda> (x y)
      ((<lambda> (<x>)
         (<set!> x y)
         (<set!> y <x>))
       x))
    1 2)

  '((<lambda> ($k x y)
      ((<lambda> ($k <x>)
         (<begin> (<set!> x y)
                  ((<lambda> $values
                     (<begin> (<set!> y <x>)
                              ($k))))))
       $k x))
    #k 1 2)

  '(load-constant 2
    load-constant 1
    load-constant #k
    load-closure
    ( load-relative (0 . 1)
      load-relative (0 . 0)
      load-closure
      ( load-relative (1 . 2)
        store-relative (1 . 1)
        drop
        load-constant #;unspecified
        load-closure
        ( load-relative (1 . 1)
          store-relative (2 . 2)
          drop
          load-constant #;unspecified
          load-relative (1 . 0)
          tail-call)
        tail-call)
      tail-call)
    tail-call)

  (if #f #f))

(check-compiler
  '(let ((x 1)
         (y 2))
     (let-syntax ((local-swap!
                    (syntax-rules ()
                      ((swap! a b)
                       (let ((x a))
                         (set! a b)
                         (set! b x))))))
       (local-swap! x y)))

  '((<lambda> (x y)
      ((<lambda> (local-swap!)
         ((<lambda> (<x>)
            (<set!> x y)
            (<set!> y <x>))
          x))))
    1 2)

  '((<lambda> ($k x y)
      ((<lambda> ($k local-swap!)
         ((<lambda> ($k <x>)
            (<begin> (<set!> x y)
                     ((<lambda> $values
                        (<begin> (<set!> y <x>)
                                 ($k))))))
          $k x))
       $k))
    #k 1 2)

  '(load-constant 2
    load-constant 1
    load-constant #k
    load-closure
    ( load-relative (0 . 0)
      load-closure
      ( load-relative (1 . 1)
        load-relative (0 . 0)
        load-closure
        ( load-relative (2 . 2)
          store-relative (2 . 1)
          drop
          load-constant #;unspecified
          load-closure
          ( load-relative (1 . 1)
            store-relative (3 . 2)
            drop
            load-constant #;unspecified
            load-relative (1 . 0)
            tail-call)
          tail-call)
        tail-call)
      tail-call)
    tail-call)

  (if #f #f))

(define-syntax aif
  (sc-macro-transformer
    (lambda (form at-use)
      (let ((test (make-syntactic-closure at-use '() (cadr form)))
            (consequent (make-syntactic-closure at-use '(it) (caddr form)))
            (alternative (if (null? (cdddr form))
                             (if #f #f)
                             (make-syntactic-closure at-use '() (cadddr form)))))
        `(let ((it ,test))
           (if it ,consequent ,alternative))))))

(check-compiler
  '(aif (memq 'b '(a b c))
        (car it))

  '((<lambda> (it)
      (if it
          (<car%-1> it)))
    (memq 'b '(a b c)))

  '(memq (<lambda> ($value)
           ((<lambda> ($k it)
              (<if> it
                    (<car%-1> $k it)
                    ($k)))
            #k
            $value))
         'b
         '(a b c))

  '(load-constant (a b c)
    load-constant b
    load-closure
    ( load-relative (0 . 0)
      load-constant #k
      load-closure
      ( load-relative (0 . 1)
        select
        ( load-relative (0 . 1)
          load-relative (0 . 0)
          load-absolute car
          tail-call)
        ( load-constant #;unspecified
          load-relative (0 . 0)
          tail-call))
      tail-call)
    load-absolute memq
    tail-call)

  'b)

(check-compiler
  '(aif (memq 'b '(a b c))
        (let ((it '(inner)))
          (car it)))

  '((<lambda> (it)
      (if it
          ((<lambda> (<it%1>)
             (<car%-1> it))
           '(inner))))
    (memq 'b '(a b c)))

  '(memq (<lambda> ($value)
           ((<lambda> ($k it)
              (<if> it
                    ((<lambda> ($k <it%1>)
                       (<car%-1> $k it))
                     $k
                     '(inner))
                    ($k)))
            #k
            $value))
         'b
         '(a b c))

  '(load-constant (a b c)
    load-constant b
    load-closure
    ( load-relative (0 . 0)
      load-constant #k
      load-closure
      ( load-relative (0 . 1)
        select
        ( load-constant (inner)
          load-relative (0 . 0)
          load-closure
          ( load-relative (1 . 1)
            load-relative (0 . 0)
            load-absolute car
            tail-call)
          tail-call)
        ( load-constant
          load-relative (0 . 0)
          tail-call))
      tail-call)
    load-absolute memq
    tail-call)

  'b)

(check-compiler ; Internal syntax definition
  '(let ()
     (define (f x y)
       (+ x y))
     (define-syntax m
       (syntax-rules ()
         ((m a b)
          (f a b))))
     (define (g x y)
       (m x y))
     (g 1 2))

  '((<lambda> ()
      ((<lambda> (f m g)
         (<set!> f (<lambda> (x y)
                     (+ x y)))
         (<set!> g (<lambda> (x y)
                     (<f> x y)))
         (g 1 2))
       ()
       ()
       ())))

  '((<lambda> ($k)
      ((<lambda> ($k f m g)
         (<begin> (<set!> f (<lambda> ($k x y)
                              (+ $k x y)))
                  ((<lambda> $values
                     (<begin> (<set!> g (<lambda> ($k x y)
                                          (<f> $k x y)))
                              ((<lambda> $values
                                 (g $k 1 2))))))))
       $k
       ()
       ()
       ()))
    #k)

  '(load-constant #k
    load-closure
    ( load-constant ()
      load-constant ()
      load-constant ()
      load-relative (0 . 0)
      load-closure
      ( load-closure
        ( load-relative (0 . 2)
          load-relative (0 . 1)
          load-relative (0 . 0)
          load-absolute +
          tail-call)
        store-relative (0 . 1)
        drop
        load-constant #;unspecified
        load-closure
        ( load-closure
          ( load-relative (0 . 2)
            load-relative (0 . 1)
            load-relative (0 . 0)
            load-relative (2 . 1)
            tail-call)
          store-relative (1 . 3)
          drop
          load-constant #;unspecified
          load-closure
          ( load-constant 2
            load-constant 1
            load-relative (2 . 0)
            load-relative (2 . 3)
            tail-call)
          tail-call)
        tail-call)
      tail-call)
    tail-call)

  '3)

(check-compiler ; Conditional expansion
  '(cond-expand
     (r5rs 'r5rs)
     (r6rs 'r6rs)
     (r7rs 'r7rs)
     (else 'unknown))

  '(<begin> 'r7rs)

  '(#k 'r7rs)

  '(load-constant r7rs
    load-constant #k
    tail-call)

  'r7rs)

(check-compiler ; Benchmark
  '(define (ack m n)
     (cond ((= m 0) (+ n 1))
           ((= n 0) (ack (- m 1) 1))
           (else (ack (- m 1) (ack m (- n 1))))))

  '(define ack
     (<lambda> (m n)
       (<if> (= m 0)
             (<begin> (+ n 1))
             (<if> (= n 0)
                   (<begin> (ack (- m 1) 1))
                   (<begin> (ack (- m 1) (ack m (- n 1))))))))

  '(<begin> (<set!> ack (<lambda> ($k m n)
                          (= (<lambda> ($value)
                               (<if> $value
                                     (+ $k n 1)
                                     (= (<lambda> ($value)
                                          (<if> $value
                                                (- (<lambda> ($value)
                                                     (ack $k $value 1))
                                                   m
                                                   1)
                                                (- (<lambda> ($value)
                                                     (- (<lambda> ($value)
                                                          (ack (<lambda> ($value)
                                                                 (ack $k $value $value))
                                                               m
                                                               $value))
                                                        n
                                                        1))
                                                   m
                                                   1)))
                                        n
                                        0)))
                             m
                             0)))
            (#k))

  '(load-closure
    ( load-constant 0
      load-relative (0 . 1)
      load-closure
      ( load-relative (0 . 0)
        select
        ( load-constant 1
          load-relative (1 . 2)
          load-relative (1 . 0)
          load-absolute +
          tail-call)
        ( load-constant 0
          load-relative (1 . 2)
          load-closure
          ( load-relative (0 . 0)
            select
            ( load-constant 1
              load-relative (2 . 1)
              load-closure
              ( load-constant 1
                load-relative (0 . 0)
                load-relative (3 . 0)
                load-absolute ack
                tail-call)
              load-absolute -
              tail-call)
            ( load-constant 1
              load-relative (2 . 1)
              load-closure
              ( load-constant 1
                load-relative (3 . 2)
                load-closure
                ( load-relative (0 . 0)
                  load-relative (4 . 1)
                  load-closure
                  ( load-relative (0 . 0)
                    load-relative (2 . 0)
                    load-relative (5 . 0)
                    load-absolute ack
                    tail-call)
                  load-absolute ack
                  tail-call)
                load-absolute -
                tail-call)
              load-absolute -
              tail-call))
          load-absolute =
          tail-call))
      load-absolute =
      tail-call)
    store-absolute ack
    drop
    load-constant #;unspecified
    load-constant #k
    tail-call)

  (if #f #f))

(check-compiler ; Benchmark
  '(define (fib n)
     (if (< n 2)
         n
         (+ (fib (- n 1))
            (fib (- n 2)))))

  '(define fib
     (<lambda> (n)
       (if (< n 2)
           n
           (+ (fib (- n 1))
              (fib (- n 2))))))

  '(<begin> (<set!> fib (<lambda> ($k n)
                          (< (<lambda> ($value)
                               (<if> $value
                                     ($k n)
                                     (- (<lambda> ($value)
                                          (fib (<lambda> ($value)
                                                 (- (<lambda> ($value)
                                                      (fib (<lambda> ($value)
                                                             (+ $k $value $value))
                                                           $value))
                                                    n
                                                    2))
                                               $value))
                                        n
                                        1)))
                             n
                             2)))
            (#k))

  '(load-closure
    ( load-constant 2
      load-relative (0 . 1)
      load-closure
      ( load-relative (0 . 0)
        select
        ( load-relative (1 . 1)
          load-relative (1 . 0)
          tail-call)
        ( load-constant 1
          load-relative (1 . 1)
          load-closure
          ( load-relative (0 . 0)
            load-closure
            ( load-constant 2
              load-relative (3 . 1)
              load-closure
              ( load-relative (0 . 0)
                load-closure
                ( load-relative (0 . 0)
                  load-relative (2 . 0)
                  load-relative (5 . 0)
                  load-absolute +
                  tail-call)
                load-absolute fib
                tail-call)
              load-absolute -
              tail-call)
            load-absolute fib
            tail-call)
          load-absolute -
          tail-call))
      load-absolute <
      tail-call)
    store-absolute fib
    drop
    load-constant #;unspecified
    load-constant #k
    tail-call)

  (if #f #f))

(check-report)

(exit (check-passed? 190))
