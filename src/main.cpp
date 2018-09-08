#include <chrono>
#include <iostream>
#include <list>
#include <sstream>
#include <string>
#include <thread>
#include <utility>

#include <boost/cstdlib.hpp>

#include <meevax/lisp/evaluator.hpp>
#include <meevax/lisp/reader.hpp>

#define LISP(...) EXPAND(__VA_ARGS__)
#define EXPAND(...) __VA_ARGS__

#define TO_STRING(...) TO_STRING_(__VA_ARGS__)
#define TO_STRING_(...) #__VA_ARGS__

#define LIST LISP( \
  (lambda (x y) (cons x (cons y (quote ())))) \
)

#define NULLQ LISP( \
  (lambda (x) (eq x (quote ()))) \
)

#define AND LISP( \
(lambda (x y) \
  (cond \
    (x (cond \
         (y (quote true)) \
         ((quote true) (quote ())))) \
    ((quote true) (quote ())))) \
)

#define NOT LISP( \
(lambda (x) \
  (cond \
    (x (quote ())) \
    ((quote true) (quote true)))) \
)

#define APPEND LISP( \
  (label append (lambda (x y) \
    (cond \
      ((NULLQ x) y) \
      ((quote true) (cons (car x) (append (cdr x) y)))))) \
)

#define ZIP LISP( \
  (label zip (lambda (x y) \
    (cond \
      ((AND (NULLQ x) (NULLQ y)) (quote ())) \
      ((AND (NOT (atom x)) (NOT (atom y))) \
       (cons (LIST (car x) (car y)) \
             (zip (cdr x) (cdr y)))) \
      ((quote true) (quote ()))))) \
)

#define CAAR LISP((lambda (x) (car (car x))))
#define CADR LISP((lambda (x) (car (cdr x))))
#define CADAR LISP((lambda (x) (car (cdr (car x)))))
#define CADDR LISP((lambda (x) (car (cdr (cdr x)))))
#define CADDAR LISP((lambda (x) (car (cdr (cdr (car x))))))

#define LOOKUP LISP( \
  (label lookup (lambda (x y) \
    (cond \
      ((NULLQ x) (quote ())) \
      ((NULLQ y) x) \
      ((quote true) \
       (cond \
         ((eq (CAAR y) x) (CADAR y)) \
         ((quote true) (lookup x (cdr y)))))))) \
)

#define EVCON LISP( \
  (label evcon (lambda (c a) \
    (cond \
      ((eval (CAAR c) a) \
       (eval (CADAR c) a)) \
      ((quote true) (evcon (cdr c) a))))) \
)

#define EVLIS LISP( \
  (label evlis (lambda (m a) \
    (cond \
      ((NULLQ m) \
       (quote ())) \
      ((quote true) \
       (cons (eval (car m) a) \
             (evlis (cdr m) a)))))) \
)

#define EVAL LISP( \
  (label eval (lambda (e a) \
    (cond \
      ((atom e) \
       (LOOKUP e a)) \
      ((atom (car e)) \
       (cond \
         ((eq (car e) (quote quote)) \
          (car (cdr e))) \
         ((eq (car e) (quote atom)) \
          (atom (eval (CADR e) a))) \
         ((eq (car e) (quote eq)) \
          (eq (eval (CADR e) a) \
              (eval (CADDR e) a))) \
         ((eq (car e) (quote car)) \
          (car (eval (CADR e) a))) \
         ((eq (car e) (quote cdr)) \
          (cdr (eval (CADR e) a))) \
         ((eq (car e) (quote cons)) \
          (cons (eval (CADR e) a) \
                (eval (CADDR e) a))) \
         ((eq (car e) (quote cond)) \
          (EVCON (cdr e) a)) \
         ((quote true) \
          (eval (cons (LOOKUP (car e) a) (cdr e)) a)))) \
      ((eq (CAAR e) (quote label)) \
       (eval (cons (CADDAR e) (cdr e)) \
             (cons (LIST (CADAR e) (car e)) a))) \
      ((eq (CAAR e) (quote lambda)) \
       (eval (CADDAR e) \
             (APPEND (ZIP (CADAR e) (EVLIS (cdr e) a)) a))) \
      ((quote true) nil)))) \
)

auto main()
  -> int
{
  using namespace meevax;

  const std::string list {TO_STRING(LIST)};
  const std::string null {TO_STRING(NULLQ)};
  const std::string and_ {TO_STRING(AND)};
  const std::string not_ {TO_STRING(NOT)};
  const std::string append {TO_STRING(APPEND)};
  const std::string pair {TO_STRING(ZIP)};
  const std::string caar {TO_STRING(CAAR)};
  const std::string assoc {TO_STRING(LOOKUP)};
  const std::string evcon {TO_STRING(EVCON)};
  const std::string evlis {TO_STRING(EVLIS)};
  const std::string eval {TO_STRING(EVAL)};

  const std::list<std::pair<std::string, std::string>> The_Roots_of_Lisp
  {
    // 1.1 quote
    {"(quote a)", "a"},
    {"(quote (a b c))", "(a . (b . (c . nil)))"},

    // 1.2 atom
    {"(atom (quote a))", "true"},
    {"(atom (quote (a b c)))", "nil"},
    {"(atom (quote ()))",  "true"},
    {"(atom (atom (quote a)))",  "true"},
    {"(atom (quote (atom (quote a))))", "nil"},

    // 1.3 eq
    {"(eq (quote a) (quote a))", "true"},
    {"(eq (quote a) (quote b))", "nil"},
    {"(eq (quote ()) (quote ()))", "true"},

    // 1.4 car
    {"(car (quote (a b c)))", "a"},

    // 1.5 cdr
    {"(cdr (quote (a b c)))", "(b . (c . nil))"},

    // 1.6 cons
    {"(cons (quote a) (quote (b c)))", "(a . (b . (c . nil)))"},
    {"(cons (quote a) (cons (quote b) (cons (quote c) (quote ()))))", "(a . (b . (c . nil)))"},
    {"(car (cons (quote a) (quote (b c))))", "a"},
    {"(cdr (cons (quote a) (quote (b c))))", "(b . (c . nil))"},

    // 1.7 cond
    {"(cond ((eq (quote a) (quote b)) (quote first)) ((atom (quote a)) (quote second)))", "second"},

    // 2.1 lambda
    {"((lambda (x) (cons x (quote (b)))) (quote a))", "(a . (b . nil))"},
    {"((lambda (x y) (cons x (cdr y))) (quote z) (quote (a b c)))", "(z . (b . (c . nil)))"},
    {"((lambda (f) (f (quote (b c)))) (quote (lambda (x) (cons (quote a) x))))",  "(a . (b . (c . nil)))"},

    // 2.2 label
    {"((label subst (lambda (x y z) (cond ((atom z) (cond ((eq z y) x) ((quote true) z))) ((quote true) (cons (subst x y (car z)) (subst x y (cdr z))))))) (quote m) (quote b) (quote (a b (a b c) d)))", "(a . (m . ((a . (m . (c . nil))) . (d . nil))))"},

    // 3.1 null
    {"(" + null + " (quote a))", "nil"},

    // 3.2 and
    {"(" + and_ + " (atom (quote a)) (eq (quote a) (quote a)))", "true"},
    {"(" + and_ + " (atom (quote a)) (eq (quote a) (quote b)))", "nil"},

    // 3.3 not
    {"(" + not_ + " (eq (quote a) (quote a)))", "nil"},
    {"(" + not_ + " (eq (quote a) (quote b)))", "true"},

    // 3.4 append
    {"(" + append + " (quote (a b)) (quote (c d)))", "(a . (b . (c . (d . nil))))"},
    {"(" + append + " (quote ()) (quote (c d)))", "(c . (d . nil))"},

    // 3.5 pair
    {"(" + pair + " (quote (x y z)) (quote (a b c)))", "((x . (a . nil)) . ((y . (b . nil)) . ((z . (c . nil)) . nil)))"},

    // 3.6 assoc
    {"(" + assoc + " (quote x) (quote ((x a) (y b))))", "a"},
    {"(" + assoc + " (quote x) (quote ((x new) (x a) (y b))))", "new"},

    // 4.1 quote
    {"(" + eval + " (quote (quote a)) (quote ()))", "a"},
    {"(" + eval + " (quote (quote (a b c))) (quote ()))", "(a . (b . (c . nil)))"},

    // 4.2 atom
    {"(" + eval + " (quote (atom (quote a))) (quote ()))", "true"},
    {"(" + eval + " (quote (atom (quote (a b c)))) (quote ()))", "nil"},
    {"(" + eval + " (quote (atom (quote ()))) (quote ()))",  "true"},
    {"(" + eval + " (quote (atom (atom (quote a)))) (quote ()))",  "true"},
    {"(" + eval + " (quote (atom (quote (atom (quote a))))) (quote ()))", "nil"},

    // 4.3 eq
    {"(" + eval + " (quote (eq (quote a) (quote a))) (quote ()))", "true"},
    {"(" + eval + " (quote (eq (quote a) (quote b))) (quote ()))", "nil"},
    {"(" + eval + " (quote (eq (quote ()) (quote ()))) (quote ()))", "true"},

    // 4.4 car
    {"(" + eval + " (quote (car (quote (a b c)))) (quote ()))", "a"},

    // 4.5 cdr
    {"(" + eval + " (quote (cdr (quote (a b c)))) (quote ()))", "(b . (c . nil))"},

    // 4.6 cons
    {"(" + eval + " (quote (cons (quote a) (quote (b c)))) (quote ()))", "(a . (b . (c . nil)))"},
    {"(" + eval + " (quote (cons (quote a) (cons (quote b) (cons (quote c) (quote ()))))) (quote ()))", "(a . (b . (c . nil)))"},
    {"(" + eval + " (quote (car (cons (quote a) (quote (b c))))) (quote ()))", "a"},
    {"(" + eval + " (quote (cdr (cons (quote a) (quote (b c))))) (quote ()))", "(b . (c . nil))"},

    // 4.7 cond
    {"(" + eval + " (quote (cond ((eq (quote a) (quote b)) (quote first)) ((atom (quote a)) (quote second)))) (quote ()))", "second"},

    // 5.1 lambda
    {"(" + eval + " (quote ((lambda (x) (cons x (quote (b)))) (quote a))) (quote ()))", "(a . (b . nil))"},
    {"(" + eval + " (quote ((lambda (x y) (cons x (cdr y))) (quote z) (quote (a b c))) (quote ()))", "(z . (b . (c . nil)))"},
    {"(" + eval + " (quote ((lambda (f) (f (quote (b c)))) (quote (lambda (x) (cons (quote a) x))))) (quote ()))",  "(a . (b . (c . nil)))"},

    // 5.2 label
    {"(" + eval + " (quote ((label subst (lambda (x y z) (cond ((atom z) (cond ((eq z y) x) ((quote true) z))) ((quote true) (cons (subst x y (car z)) (subst x y (cdr z))))))) (quote m) (quote b) (quote (a b (a b c) d)))) (quote ()))", "(a . (m . ((a . (m . (c . nil))) . (d . nil))))"},
  };

  for (const auto& [test, answer] : The_Roots_of_Lisp)
  {
    std::stringstream ss {};
    ss << lisp::eval(lisp::read(test));

    std::cerr << "\n"
              << "test: \e[32m" << test << "\e[0m\n"
              << "  -> \e[36m" << ss.str() << "\e[0m\n"
              << "  -> \e[31m";

    if (ss.str() == answer)
    {
      std::cerr << "success" << "\e[0m\n";
    }
    else
    {
      std::cerr << "failed" << "\e[0m\n"
                << "  -> " << answer << " expected" << std::endl;
      std::exit(boost::exit_failure);
    }

    std::this_thread::sleep_for(std::chrono::milliseconds {100});
  }

  std::cerr << "\n"
            << "all tests passed." << std::endl;

  for (std::string buffer {}; std::cout << "\n>> ", std::getline(std::cin, buffer); )
  {
    std::cout << lisp::eval(lisp::read(buffer)) << std::endl;
  }

  return boost::exit_success;
}

