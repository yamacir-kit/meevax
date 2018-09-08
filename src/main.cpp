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

auto main()
  -> int
{
  using namespace meevax;

  const std::string list {" \
    (lambda (x y) (cons x (cons y nil))) \
  "};

  const std::string null {" \
    (lambda (x) (eq x (quote ()))) \
  "};

  const std::string and_ {" \
    (lambda (x y) \
      (cond \
        (x (cond \
             (y (quote true)) \
             ((quote true) (quote ())))) \
        ((quote true) (quote ())))) \
  "};

  const std::string not_ {" \
    (lambda (x) \
      (cond \
        (x (quote ())) \
        ((quote true) (quote true)))) \
  "};

  const std::string append {" \
    (label append (lambda (x y) \
      (cond \
        ((" + null + " x) y) \
        ((quote true) (cons (car x) (append (cdr x) y)))))) \
  "};

  const std::string pair {" \
    (label pair (lambda (x y) \
      (cond \
        ((" + and_ + " (" + null + " x) (" + null + " y)) (quote ())) \
        ((" + and_ + " (" + not_ + " (atom x)) (" + not_ + " (atom y))) \
         (cons (" + list + " (car x) (car y)) \
               (    pair     (cdr x) (cdr y)))) \
        ((quote true) (quote ()))))) \
  "};

  const std::string caar {" \
    (lambda (x) (car (car x))) \
  "};

  const std::string assoc {" \
    (label assoc (lambda (x y) \
      (cond \
        ((" + null + " x) (quote ())) \
        ((" + null + " y) x) \
        ((quote true) \
         (cond \
           ((eq (car (car y)) x) (car (cdr (car y)))) \
           ((quote true) (assoc x (cdr y)))))))) \
  "};

  const std::string evcon {" \
    (label evcon (lambda (c a) \
      (cond \
        ((eval (car (car c)) a) \
         (eval (car (cdr (car c))) a)) \
        ((quote true) (evcon (cdr c) a))))) \
  "};

  const std::string evlis {" \
    (label evlis (lambda (m a) \
      (cond \
        ((" + null + " m) \
         (quote ())) \
        ((quote true) \
         (cons (eval (car m) a) \
               (evlis (cdr m) a)))))) \
  "};

  const std::string eval {" \
    (label eval (lambda (e a) \
      (cond \
        ((atom e) \
         (" + assoc + " e a)) \
        ((atom (car e)) \
         (cond \
           ((eq (car e) (quote quote)) \
            (car (cdr e))) \
           ((eq (car e) (quote atom)) \
            (atom (eval (car (cdr e)) a))) \
           ((eq (car e) (quote eq)) \
            (eq (eval (car (cdr e)) a) \
                (eval (car (cdr (cdr e))) a))) \
           ((eq (car e) (quote car)) \
            (car (eval (car (cdr e)) a))) \
           ((eq (car e) (quote cdr)) \
            (cdr (eval (car (cdr e)) a))) \
           ((eq (car e) (quote cons)) \
            (cons (eval (car (cdr e)) a) \
                  (eval (car (cdr (cdr e))) a))) \
           ((eq (car e) (quote cond)) \
            (" + evcon + " (cdr e) a)) \
           ((quote true) \
            (eval (cons (" + assoc + " (car e) a) (cdr e)) a)))) \
        ((eq (car (car e)) (quote label)) \
         (eval (cons (car (cdr (cdr (car e)))) (cdr e)) \
               (cons (" + list + " (car (cdr (car e))) (car e)) a))) \
        ((eq (car (car e)) (quote lambda)) \
         (eval (car (cdr (cdr (car e)))) \
               (" + append + " (" + pair + " (car (cdr (car e))) \
                                             (" + evlis + " (cdr e) a)) \
                               a))) \
        (true nil)))) \
  "};

  const std::list<std::pair<std::string, std::string>> The_Roots_of_Lisp
  {
  // Seven Primitive Operators
    // 1. quote
    {"(quote a)", "a"},
    {"(quote (a b c))", "(a . (b . (c . nil)))"},

    // 2. atom
    {"(atom (quote a))", "true"},
    {"(atom (quote (a b c)))", "nil"},
    {"(atom (quote ()))",  "true"},
    {"(atom (atom (quote a)))",  "true"},
    {"(atom (quote (atom (quote a))))", "nil"},

    // 3. eq
    {"(eq (quote a) (quote a))", "true"},
    {"(eq (quote a) (quote b))", "nil"},
    {"(eq (quote ()) (quote ()))", "true"},

    // 4. car
    {"(car (quote (a b c)))", "a"},

    // 5. cdr
    {"(cdr (quote (a b c)))", "(b . (c . nil))"},

    // 6. cons
    {"(cons (quote a) (quote (b c)))", "(a . (b . (c . nil)))"},
    {"(cons (quote a) (cons (quote b) (cons (quote c) (quote ()))))", "(a . (b . (c . nil)))"},
    {"(car (cons (quote a) (quote (b c))))", "a"},
    {"(cdr (cons (quote a) (quote (b c))))", "(b . (c . nil))"},

    // 7. cond
    {"(cond ((eq (quote a) (quote b)) (quote first)) ((atom (quote a)) (quote second)))", "second"},

  // Denoting Functions
    // 1. lambda
    {"((lambda (x) (cons x (quote (b)))) (quote a))", "(a . (b . nil))"},
    {"((lambda (x y) (cons x (cdr y))) (quote z) (quote (a b c)))", "(z . (b . (c . nil)))"},
    {"((lambda (f) (f (quote (b c)))) (quote (lambda (x) (cons (quote a) x))))",  "(a . (b . (c . nil)))"},

    // 2. label
    {"((label subst (lambda (x y z) (cond ((atom z) (cond ((eq z y) x) ((quote true) z))) ((quote true) (cons (subst x y (car z)) (subst x y (cdr z))))))) (quote m) (quote b) (quote (a b (a b c) d)))", "(a . (m . ((a . (m . (c . nil))) . (d . nil))))"},

  // Some Functions
    // 1. null
    {"(" + null + " (quote a))", "nil"},

    // 2. and
    {"(" + and_ + " (atom (quote a)) (eq (quote a) (quote a)))", "true"},
    {"(" + and_ + " (atom (quote a)) (eq (quote a) (quote b)))", "nil"},

    // 3. not
    {"(" + not_ + " (eq (quote a) (quote a)))", "nil"},
    {"(" + not_ + " (eq (quote a) (quote b)))", "true"},

    // 4. append
    {"(" + append + " (quote (a b)) (quote (c d)))", "(a . (b . (c . (d . nil))))"},
    {"(" + append + " (quote ()) (quote (c d)))", "(c . (d . nil))"},

    // 5. pair
    {"(" + pair + " (quote (x y z)) (quote (a b c)))", "((x . (a . nil)) . ((y . (b . nil)) . ((z . (c . nil)) . nil)))"},

    // 6. assoc
    {"(" + assoc + " (quote x) (quote ((x a) (y b))))", "a"},
    {"(" + assoc + " (quote x) (quote ((x new) (x a) (y b))))", "new"},

  // Meta-Circular Evaluator
    // 1. quote
    {"(" + eval + " (quote (quote a)) (quote ()))", "a"},
    {"(" + eval + " (quote (quote (a b c))) (quote ()))", "(a . (b . (c . nil)))"},

    // 2. atom
    {"(" + eval + " (quote (atom (quote a))) (quote ()))", "true"},
    {"(" + eval + " (quote (atom (quote (a b c)))) (quote ()))", "nil"},
    {"(" + eval + " (quote (atom (quote ()))) (quote ()))",  "true"},
    {"(" + eval + " (quote (atom (atom (quote a)))) (quote ()))",  "true"},
    {"(" + eval + " (quote (atom (quote (atom (quote a))))) (quote ()))", "nil"},

    // 3. eq
    {"(" + eval + " (quote (eq (quote a) (quote a))) (quote ()))", "true"},
    {"(" + eval + " (quote (eq (quote a) (quote b))) (quote ()))", "nil"},
    {"(" + eval + " (quote (eq (quote ()) (quote ()))) (quote ()))", "true"},

    // 4. car
    {"(" + eval + " (quote (car (quote (a b c)))) (quote ()))", "a"},

    // 5. cdr
    {"(" + eval + " (quote (cdr (quote (a b c)))) (quote ()))", "(b . (c . nil))"},

    // 6. cons
    {"(" + eval + " (quote (cons (quote a) (quote (b c)))) (quote ()))", "(a . (b . (c . nil)))"},
    {"(" + eval + " (quote (cons (quote a) (cons (quote b) (cons (quote c) (quote ()))))) (quote ()))", "(a . (b . (c . nil)))"},
    {"(" + eval + " (quote (car (cons (quote a) (quote (b c))))) (quote ()))", "a"},
    {"(" + eval + " (quote (cdr (cons (quote a) (quote (b c))))) (quote ()))", "(b . (c . nil))"},

    // 7. cond
    {"(" + eval + " (quote (cond ((eq (quote a) (quote b)) (quote first)) ((atom (quote a)) (quote second)))) (quote ()))", "second"},

    // ()
    {"", "nil"}
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

    // std::this_thread::sleep_for(std::chrono::milliseconds {100});
  }

  std::cerr << "\n"
            << "all tests passed." << std::endl;

  for (std::string buffer {}; std::cout << "\n>> ", std::getline(std::cin, buffer); )
  {
    std::cout << lisp::eval(lisp::read(buffer)) << std::endl;
  }

  return boost::exit_success;
}

