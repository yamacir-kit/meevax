#ifndef INCLUDED_MEEVAX_LISP_EVALUATOR_HPP
#define INCLUDED_MEEVAX_LISP_EVALUATOR_HPP

#include <functional>
#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>

#include <meevax/lisp/accessor.hpp>
#include <meevax/lisp/cell.hpp>
#include <meevax/lisp/error.hpp>
#include <meevax/lisp/function.hpp>

#define define(SYMBOL, ...) \
  env = cons(list(s[SYMBOL], cell::make_as<procedure>(__VA_ARGS__)), env);

namespace meevax::lisp
{
  class evaluator
  {
    static inline auto env {cell::nil};

    using procedure = std::function<
                        const std::shared_ptr<cell> (const std::shared_ptr<cell>&,
                                                     const std::shared_ptr<cell>&)
                      >;

  public:
    evaluator()
    {
      define("quote", [](const auto& e, const auto&)
      {
        return cadr(e);
      });

      define("atom", [&](const auto& e, const auto& a)
      {
        return atom(eval(cadr(e), a)) ? s["true"] : cell::nil;
      });

      define("eq", [&](const auto& e, const auto& a)
      {
        return eq(eval(cadr(e), a), eval(caddr(e), a)) ? s["true"] : cell::nil;
      });

      define("cond", [&](const auto& e, const auto& a)
      {
        return evcon(cdr(e), a);
      });

      define("car", [&](const auto& e, const auto& a)
      {
        return car(eval(cadr(e), a));
      });

      define("cdr", [&](const auto& e, const auto& a)
      {
        return cdr(eval(cadr(e), a));
      });

      define("cons", [&](const auto& e, const auto& a)
      {
        return cons(eval(cadr(e), a), eval(caddr(e), a));
      });

      define("define", [&](const auto& e, const auto& a)
      {
        a = cons(list(cadr(e), caddr(e)), env);
        return assoc(cadr(e), a);
      });
    }

    static inline std::unordered_map<std::string, std::shared_ptr<cell>> s
    {
      {"",       cell::nil},
      {"atom",   cell::make_as<std::string>("atom")},
      {"car",    cell::make_as<std::string>("car")},
      {"cdr",    cell::make_as<std::string>("cdr")},
      {"cond",   cell::make_as<std::string>("cond")},
      {"cons",   cell::make_as<std::string>("cons")},
      {"define", cell::make_as<std::string>("define")},
      {"eq",     cell::make_as<std::string>("eq")},
      {"label",  cell::make_as<std::string>("label")},
      {"lambda", cell::make_as<std::string>("lambda")},
      {"nil",    cell::nil},
      {"quote",  cell::make_as<std::string>("quote")},
      {"true",   cell::make_as<std::string>("true")}
    };

    decltype(auto) operator()(const std::shared_ptr<cell>& e)
    {
      return eval(e, env);
    }

  protected:
    auto eval(const std::shared_ptr<cell>& e, const std::shared_ptr<cell>& a)
      -> const std::shared_ptr<cell>
    {
      if (atom(e))
      {
        return assoc(e, a);
      }
      else if (atom(car(e)))
      {
        if (auto hoge {assoc(car(e), a)}; hoge->type() == typeid(procedure))
        {
          return hoge->as<procedure>()(e, a);
        }
        else if (atom(hoge))
        {
          std::cerr << error("using atom \"" << hoge << "\" as procedure") << std::endl;
          return cell::nil;
        }
        else
        {
          return eval(cons(hoge, cdr(e)), a);
        }
      }
      else if (eq(caar(e), s["label"]))
      {
        return eval(cons(caddar(e), cdr(e)), cons(list(cadar(e), car(e)), a));
      }
      else if (eq(caar(e), s["lambda"]))
      {
        return eval(caddar(e), append(zip(cadar(e), evlis(cdr(e), a)), a));
      }
      else
      {
        std::cerr << error("unknown function \"" << car(e) << "\"") << std::endl;
        return cell::nil;
      }
    }

    auto evcon(const std::shared_ptr<cell>& c, const std::shared_ptr<cell>& a)
      -> const std::shared_ptr<cell>
    {
      return not eq(eval(caar(c), a), cell::nil) ? eval(cadar(c), a) : evcon(cdr(c), a);
    }

    auto evlis(const std::shared_ptr<cell>& m, const std::shared_ptr<cell>& a)
      -> const std::shared_ptr<cell>
    {
      return null(m) ? cell::nil : cons(eval(car(m), a), evlis(cdr(m), a));
    }
  } static eval {};
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_EVALUATOR_HPP

