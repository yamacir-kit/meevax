#ifndef INCLUDED_MEEVAX_DEBUG_THE_ROOTS_OF_LISP_HPP
#define INCLUDED_MEEVAX_DEBUG_THE_ROOTS_OF_LISP_HPP

#include <list>
#include <string>
#include <utility>

#define LISP(...) #__VA_ARGS__

namespace meevax::debug
{
  const std::list<std::pair<std::string, std::string>> The_Roots_of_Lisp
  {
    // 1.1 quote
    {
      LISP(
        (quote a)
      ),

      "a"
    },

    {
      LISP(
        (quote (a b c))
      ),

      "(a b c)"
    },

    // 1.2 atom
    {
      LISP(
        (atom (quote a))
      ),

      "true"
    },

    {
      LISP(
        (atom (quote (a b c)))
      ),

      "nil"
    },

    {
      LISP(
        (atom (quote ()))
      ),

      "true"
    },

    {
      LISP(
        (atom (atom (quote a)))
      ),

      "true"
    },

    {
      LISP(
        (atom (quote (atom (quote a))))
      ),

      "nil"
    },

    // 1.3 eq
    {
      LISP(
        (eq (quote a) (quote a))
      ),

      "true"
    },

    {
      LISP(
        (eq (quote a) (quote b))
      ),

      "nil"
    },

    {
      LISP(
        (eq (quote ()) (quote ()))
      ),

      "true"
    },

    // 1.4 car
    {
      LISP(
        (car (quote (a b c)))
      ),

      "a"
    },

    // 1.5 cdr
    {
      LISP(
        (cdr (quote (a b c)))
      ),

      "(b c)"
    },

    // 1.6 cons
    {
      LISP(
        (cons (quote a)
              (quote (b c)))
       ),

      "(a b c)"
    },

    {
      LISP(
        (cons (quote a)
              (cons (quote b)
                    (cons (quote c)
                          (quote ()))))
      ),

      "(a b c)"
    },

    {
      LISP(
        (car (cons (quote a)
                   (quote (b c))))
      ),

      "a"
    },

    {
      LISP(
        (cdr (cons (quote a)
                   (quote (b c))))
      ),

      "(b c)"
    },

    // 1.7 cond
    {
      LISP(
        (cond
          ((eq (quote a) (quote b))
           (quote first))
          ((atom (quote a))
           (quote second)))
      ),

      "second"
    },

    // 2.1 lambda
    {
      LISP(
        ((lambda (x) (cons x (quote (b))))
         (quote a))
      ),

      "(a b)"
    },

    {
      LISP(
        ((lambda (x y) (cons x (cdr y))) (quote z)
         (quote (a b c)))
      ),

      "(z b c)"
    },

    {
      LISP(
        ((lambda (f)
           (f (quote (b c))))
         (quote (lambda (x)
                  (cons (quote a) x))))
      ),

      "(a b c)"
    },

    // 2.2 label
    {
      LISP(
        ((label subst (lambda (x y z)
           (cond
             ((atom z)
              (cond
                ((eq z y) x)
                ((quote true) z)))
             ((quote true)
              (cons (subst x y (car z))
                    (subst x y (cdr z)))))))
         (quote m)
         (quote b)
         (quote (a b (a b c) d)))
      ),

      "(a m (a m c) d)"
    },

    // 3.1 null
    {
      LISP(
        (define null (lambda (x)
          (eq x (quote ()))))
      ),

      "null"
    },

    {
      LISP(
        (null (quote a))
      ),

      "nil"
    },

    // 3.2 and
    {
      LISP(
        (define and (lambda (x y)
          (cond
            (x (cond
                 (y (quote true))
                 ((quote true) (quote ()))))
            ((quote true) (quote ())))))
      ),

      "and"
    },

    {
      LISP(
        (and (atom (quote a))
             (eq (quote a) (quote a)))
      ),

      "true"
    },

    {
      LISP(
        (and (atom (quote a))
             (eq (quote a) (quote b)))
      ),

      "nil"
    },

    // 3.3 not
    {
      LISP(
        (define not (lambda (x)
          (cond
            (x (quote ()))
            ((quote true) (quote true)))))
      ),

      "not"
    },

    {
      LISP(
        (not (eq (quote a) (quote a)))
      ),

      "nil"
    },

    {
      LISP(
        (not (eq (quote a) (quote b)))
      ),

      "true"
    },

    // 3.4 append
    {
      LISP(
        (define append (lambda (x y)
          (cond
            ((null x) y)
            ((quote true) (cons (car x) (append (cdr x) y))))))
      ),

      "append"
    },

    {
      LISP(
        (append (quote (a b)) (quote (c d)))
      ),

      "(a b c d)"
    },

    {
      LISP(
        (append (quote ()) (quote (c d)))
      ),

      "(c d)"
    },

    // 3.5 zip
    {
      LISP(
        (define list (lambda (x y)
          (cons x (cons y (quote ())))))
      ),

      "list"
    },

    {
      LISP(
        (define zip (lambda (x y)
          (cond
            ((and (null x) (null y))
             (quote ()))
            ((and (not (atom x))(not (atom y)))
             (cons (list (car x) (car y))
                   (zip (cdr x) (cdr y))))
            ((quote true) (quote ())))))
      ),

      "zip"
    },

    {
      LISP(
        (zip (quote (x y z))
             (quote (a b c)))
      ),

      "((x a) (y b) (z c))"
    },

    // 3.6 assoc
    {
      LISP(
        (define caar (lambda (x)
          (car (car x))))
      ),

      "caar"
    },

    {
      LISP(
        (define cadr (lambda (x)
          (car (cdr x))))
      ),

      "cadr"
    },

    {
      LISP(
        (define cadar (lambda (x)
          (car (cdr (car x)))))
      ),

      "cadar"
    },

    {
      LISP(
        (define caddr (lambda (x)
          (car (cdr (cdr x)))))
      ),

      "caddr"
    },

    {
      LISP(
        (define caddar (lambda (x)
          (car (cdr (cdr (car x))))))
      ),

      "caddar"
    },

    {
      LISP(
        (define assoc (lambda (x y)
          (cond
            ((null x) (quote ()))
            ((null y) x)
            ((quote true)
             (cond
               ((eq (caar y) x) (cadar y))
               ((quote true) (assoc x (cdr y))))))))
      ),

      "assoc"
    },

    {
      LISP(
        (assoc (quote x)
               (quote ((x a) (y b))))
      ),

      "a"
    },

    {
      LISP(
        (assoc (quote x)
               (quote ((x new) (x a) (y b))))
      ),

      "new"
    },

    // 4.1 evcon
    {
      LISP(
        (define evcon (lambda (c a)
          (cond
            ((eval (caar c) a)
             (eval (cadar c) a))
            ((quote true) (evcon (cdr c) a)))))
      ),

      "evcon"
    },

    // 4.2 evlis
    {
      LISP(
        (define evlis (lambda (m a)
          (cond
            ((null m) (quote ()))
            ((quote true)
             (cons (eval (car m) a)
                   (evlis (cdr m) a))))))
      ),

      "evlis"
    },

    // 4.3 eval
    {
      LISP(
        (define eval (lambda (e a)
          (cond
            ((atom e)
             (assoc e a))
            ((atom (car e))
             (cond
               ((eq (car e) (quote quote))
                (car (cdr e)))
               ((eq (car e) (quote atom))
                (atom (eval (cadr e) a)))
               ((eq (car e) (quote eq))
                (eq (eval (cadr e) a) (eval (caddr e) a)))
               ((eq (car e) (quote car))
                (car (eval (cadr e) a)))
               ((eq (car e) (quote cdr))
                (cdr (eval (cadr e) a)))
               ((eq (car e) (quote cons))
                (cons (eval (cadr e) a) (eval (caddr e) a)))
               ((eq (car e) (quote cond))
                (evcon (cdr e) a))
               ((quote true)
                (eval (cons (assoc (car e) a) (cdr e)) a))))
            ((eq (caar e) (quote label))
             (eval (cons (caddar e) (cdr e)) (cons (list (cadar e) (car e)) a)))
            ((eq (caar e) (quote lambda))
             (eval (caddar e) (append (zip (cadar e) (evlis (cdr e) a)) a)))
            ((quote true) (quote error)))))
      ),

      "eval"
    },

    // Ex1.1 quote
    {
      LISP(
        (eval (quote (quote a))
              (quote ()))
      ),

      "a"
    },

    {
      LISP(
        (eval (quote (quote (a b c)))
              (quote ()))
      ),

      "(a b c)"
    },

    // Ex1.2 atom
    {
      LISP(
        (eval (quote (atom (quote a)))
              (quote ()))
      ),

      "true"
    },

    {
      LISP(
        (eval (quote (atom (quote (a b c))))
              (quote ()))
      ),

      "nil"
    },

    {
      LISP(
        (eval (quote (atom (quote ())))
              (quote ()))
      ),

      "true"
    },

    {
      LISP(
        (eval (quote (atom (atom (quote a))))
              (quote ()))
      ),

      "true"
    },

    {
      LISP(
        (eval (quote (atom (quote (atom (quote a)))))
              (quote ()))
      ),

      "nil"
    },

    // Ex1.3 eq
    {
      LISP(
        (eval (quote (eq (quote a) (quote a)))
              (quote ()))
      ),

      "true"
    },

    {
      LISP(
        (eval (quote (eq (quote a) (quote b)))
              (quote ()))
      ),

      "nil"
    },

    {
      LISP(
        (eval (quote (eq (quote ()) (quote ())))
              (quote ()))
      ),

      "true"
    },

    // Ex1.4 car
    {
      LISP(
        (eval (quote (car (quote (a b c))))
              (quote ()))
      ),

      "a"
    },

    // Ex1.5 cdr
    {
      LISP(
        (eval (quote (cdr (quote (a b c))))
              (quote ()))
      ),

      "(b c)"
    },

    // Ex1.6 cons
    {
      LISP(
        (eval (quote (cons (quote a) (quote (b c))))
              (quote ()))
      ),

      "(a b c)"
    },

    {
      LISP(
        (eval (quote (cons (quote a) (cons (quote b) (cons (quote c) (quote ())))))
              (quote ()))
      ),

      "(a b c)"
    },

    {
      LISP(
        (eval (quote (car (cons (quote a) (quote (b c)))))
              (quote ()))
      ),

      "a"
    },

    {
      LISP(
        (eval (quote (cdr (cons (quote a) (quote (b c)))))
              (quote ()))
      ),

      "(b c)"
    },

    // Ex1.7 cond
    {
      LISP(
        (eval (quote (cond
                       ((eq (quote a) (quote b))
                        (quote first))
                       ((atom (quote a))
                        (quote second))))
              (quote ()))
      ),

      "second"
    },

    // Ex2.1 lambda
    {
      LISP(
        (eval (quote ((lambda (x) (cons x (quote (b)))) (quote a)))
              (quote ()))
      ),

      "(a b)"
    },

    {
      LISP(
        (eval (quote ((lambda (x y)
                        (cons x (cdr y)))
                      (quote z)
                      (quote (a b c)))
              (quote ())))
      ),

      "(z b c)"
    },

    {
      LISP(
        (eval (quote ((lambda (f)
                        (f (quote (b c))))
                      (quote (lambda (x) (cons (quote a) x)))))
              (quote ()))
      ),

      "(a b c)"
    },

    // Ex2.2 label
    {
      LISP(
        (eval (quote ((label subst (lambda (x y z)
                        (cond
                          ((atom z)
                           (cond
                             ((eq z y) x)
                             ((quote true) z)))
                          ((quote true)
                           (cons (subst x y (car z))
                                 (subst x y (cdr z)))))))
                      (quote m)
                      (quote b)
                      (quote (a b (a b c) d))))
              (quote ()))
      ),

      "(a m (a m c) d)"
    }
  };
} // namespace meevax::debug

#endif // INCLUDED_MEEVAX_DEBUG_THE_ROOTS_OF_LISP_HPP

