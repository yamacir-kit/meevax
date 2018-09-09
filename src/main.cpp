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

#define EXPAND(...) EXPAND_(__VA_ARGS__)
#define EXPAND_(...) __VA_ARGS__

#define S(...) S_(__VA_ARGS__)
#define S_(...) #__VA_ARGS__

#define CAAR (lambda (x) (car (car x)))
#define CADR (lambda (x) (car (cdr x)))
#define CADAR (lambda (x) (car (cdr (car x))))
#define CADDR (lambda (x) (car (cdr (cdr x))))
#define CADDAR (lambda (x) (car (cdr (cdr (car x)))))

#define LIST (lambda (x y) (cons x (cons y (quote ()))))
#define NULLQ (lambda (x) (eq x (quote ())))
#define AND (lambda (x y) (cond (x (cond (y (quote true)) ((quote true) (quote ())))) ((quote true) (quote ()))))
#define NOT (lambda (x) (cond (x (quote ())) ((quote true) (quote true))))

#define APPEND EXPAND( \
  (label append (lambda (x y) \
    (cond \
      ((NULLQ x) y) \
      ((quote true) (cons (car x) (append (cdr x) y)))))) \
)

#define ZIP EXPAND( \
  (label zip (lambda (x y) \
    (cond \
      ((AND (NULLQ x) (NULLQ y)) (quote ())) \
      ((AND (NOT (atom x)) (NOT (atom y))) \
       (cons (LIST (car x) (car y)) \
             (zip (cdr x) (cdr y)))) \
      ((quote true) (quote ()))))) \
)

#define LOOKUP EXPAND( \
  (label lookup (lambda (x y) \
    (cond \
      ((NULLQ x) (quote ())) \
      ((NULLQ y) x) \
      ((quote true) \
       (cond \
         ((eq (CAAR y) x) (CADAR y)) \
         ((quote true) (lookup x (cdr y)))))))) \
)

#define EVCON EXPAND( \
  (label evcon (lambda (c a) \
    (cond \
      ((eval (CAAR c) a) \
       (eval (CADAR c) a)) \
      ((quote true) (evcon (cdr c) a))))) \
)

#define EVLIS EXPAND( \
  (label evlis (lambda (m a) \
    (cond \
      ((NULLQ m) (quote ())) \
      ((quote true) \
       (cons (eval (car m) a) \
             (evlis (cdr m) a)))))) \
)

#define EVAL EXPAND( \
  (label eval (lambda (e a) \
    (cond \
      ((atom e) (LOOKUP e a)) \
      ((atom (car e)) \
       (cond \
         ((eq (car e) (quote quote)) (car (cdr e))) \
         ((eq (car e) (quote atom)) (atom (eval (CADR e) a))) \
         ((eq (car e) (quote eq)) (eq (eval (CADR e) a) \
                                      (eval (CADDR e) a))) \
         ((eq (car e) (quote car)) (car (eval (CADR e) a))) \
         ((eq (car e) (quote cdr)) (cdr (eval (CADR e) a))) \
         ((eq (car e) (quote cons)) (cons (eval (CADR e) a) \
                                          (eval (CADDR e) a))) \
         ((eq (car e) (quote cond)) (EVCON (cdr e) a)) \
         ((quote true) (eval (cons (LOOKUP (car e) a) (cdr e)) a)))) \
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
    {"(define null (lambda (x) (eq x (quote ()))))", ""},
    {"(null (quote a))", "nil"},

    // 3.2 and
    {"(define and (lambda (x y) \
        (cond \
          (x (cond \
               (y (quote true)) \
               ((quote true) (quote ())))) \
          ((quote true) (quote ()))))) \
     ", ""},
    {"(and (atom (quote a)) (eq (quote a) (quote a)))", "true"},
    {"(and (atom (quote a)) (eq (quote a) (quote b)))", "nil"},

    // 3.3 not
    {S((NOT (eq (quote a) (quote a)))), "nil"},
    {S((NOT (eq (quote a) (quote b)))), "true"},

    // 3.4 append
    {S((APPEND (quote (a b)) (quote (c d)))), "(a . (b . (c . (d . nil))))"},
    {S((APPEND (quote ()) (quote (c d)))), "(c . (d . nil))"},

    // 3.5 pair
    {S((ZIP (quote (x y z)) (quote (a b c)))), "((x . (a . nil)) . ((y . (b . nil)) . ((z . (c . nil)) . nil)))"},

    // 3.6 assoc
    {S((LOOKUP (quote x) (quote ((x a) (y b))))), "a"},
    {S((LOOKUP (quote x) (quote ((x new) (x a) (y b))))), "new"},

    // 4.1 quote
    {S((EVAL (quote (quote a)) (quote ()))), "a"},
    {S((EVAL (quote (quote (a b c))) (quote ()))), "(a . (b . (c . nil)))"},

    // 4.2 atom
    {S((EVAL (quote (atom (quote a))) (quote ()))), "true"},
    {S((EVAL (quote (atom (quote (a b c)))) (quote ()))), "nil"},
    {S((EVAL (quote (atom (quote ()))) (quote ()))),  "true"},
    {S((EVAL (quote (atom (atom (quote a)))) (quote ()))),  "true"},
    {S((EVAL (quote (atom (quote (atom (quote a))))) (quote ()))), "nil"},

    // 4.3 eq
    {S((EVAL (quote (eq (quote a) (quote a))) (quote ()))), "true"},
    {S((EVAL (quote (eq (quote a) (quote b))) (quote ()))), "nil"},
    {S((EVAL (quote (eq (quote ()) (quote ()))) (quote ()))), "true"},

    // 4.4 car
    {S((EVAL (quote (car (quote (a b c)))) (quote ()))), "a"},

    // 4.5 cdr
    {S((EVAL (quote (cdr (quote (a b c)))) (quote ()))), "(b . (c . nil))"},

    // 4.6 cons
    {S((EVAL (quote (cons (quote a) (quote (b c)))) (quote ()))), "(a . (b . (c . nil)))"},
    {S((EVAL (quote (cons (quote a) (cons (quote b) (cons (quote c) (quote ()))))) (quote ()))), "(a . (b . (c . nil)))"},
    {S((EVAL (quote (car (cons (quote a) (quote (b c))))) (quote ()))), "a"},
    {S((EVAL (quote (cdr (cons (quote a) (quote (b c))))) (quote ()))), "(b . (c . nil))"},

    // 4.7 cond
    {S((EVAL (quote (cond ((eq (quote a) (quote b)) (quote first)) ((atom (quote a)) (quote second)))) (quote ()))), "second"},

    // 5.1 lambda
    {S((EVAL (quote ((lambda (x) (cons x (quote (b)))) (quote a))) (quote ()))), "(a . (b . nil))"},
    {S((EVAL (quote ((lambda (x y) (cons x (cdr y))) (quote z) (quote (a b c))) (quote ())))), "(z . (b . (c . nil)))"},
    {S((EVAL (quote ((lambda (f) (f (quote (b c)))) (quote (lambda (x) (cons (quote a) x))))) (quote ()))),  "(a . (b . (c . nil)))"},

    // 5.2 label
    {S((EVAL (quote ((label subst (lambda (x y z) (cond ((atom z) (cond ((eq z y) x) ((quote true) z))) ((quote true) (cons (subst x y (car z)) (subst x y (cdr z))))))) (quote m) (quote b) (quote (a b (a b c) d)))) (quote ()))), "(a . (m . ((a . (m . (c . nil))) . (d . nil))))"},
  };

  for (const auto& [test, answer] : The_Roots_of_Lisp)
  {
    std::stringstream ss {};
    ss << lisp::eval(lisp::read(test));

    std::cerr << "\ntest: \e[32m" << test << "\e[0m\n"
              << "  -> \e[36m" << ss.str() << "\e[0m\n"
              << "  -> \e[31m";

    if (std::empty(answer) || ss.str() == answer)
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

  std::cerr << "\nall tests passed." << std::endl;

  for (std::string buffer {}; std::cout << "\n>> ", std::getline(std::cin, buffer); )
  {
    std::cout << lisp::eval(lisp::read(buffer)) << std::endl;
  }

  return boost::exit_success;
}

