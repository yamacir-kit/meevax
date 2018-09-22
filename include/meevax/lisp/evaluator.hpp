#ifndef INCLUDED_MEEVAX_LISP_EVALUATOR_HPP
#define INCLUDED_MEEVAX_LISP_EVALUATOR_HPP

#include <iostream>
#include <memory>
#include <string>
#include <utility>

#include <meevax/lisp/accessor.hpp>
#include <meevax/lisp/alias.hpp>
#include <meevax/lisp/cell.hpp>
#include <meevax/lisp/error.hpp>
#include <meevax/lisp/table.hpp>

// TODO evaluator::define()
#define define(SYMBOL, ...) \
  env = cons( \
          list( \
            symbols.intern(SYMBOL), \
            cell::make_as<special>(__VA_ARGS__) \
          ), \
          env \
        );

namespace meevax::lisp
{
  class evaluator
  {
    static inline auto env {symbols.intern("nil")};

  public:
    evaluator()
    {
      define("quote", [](const auto& e, const auto&)
      {
        return cadr(e);
      });

      define("atom", [&](const auto& e, const auto& a)
      {
        return atom(eval(cadr(e), a))
                 ? symbols.intern("true")
                 : symbols.intern("nil");
      });

      define("eq", [&](const auto& e, const auto& a)
      {
        return eq(eval(cadr(e), a), eval(caddr(e), a))
                 ? symbols.intern("true")
                 : symbols.intern("nil");
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
        env = cons(list(cadr(e), caddr(e)), env);
        return assoc(cadr(e), a);
      });
    }

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
        if (auto procedure {assoc(car(e), a)}; procedure->type() == typeid(special))
        {
          return procedure->as<special>()(e, a);
        }
        else if (atom(procedure))
        {
          std::cerr << error("using atom \"" << procedure << "\" as procedure") << std::endl;
          return cell::nil;
        }
        else
        {
          return eval(cons(procedure, cdr(e)), a);
        }
      }
      else if (eq(caar(e), symbols.intern("label")))
      {
        return eval(cons(caddar(e), cdr(e)), cons(list(cadar(e), car(e)), a));
      }
      else if (eq(caar(e), symbols.intern("lambda")))
      {
        return eval(caddar(e), append(zip(cadar(e), evlis(cdr(e), a)), a));
      }
      else
      {
        std::cerr << error("unknown function \"" << car(e) << "\"") << std::endl;
        return cell::nil;
      }
    }

  private:
    // TODO convert to cell::operator bool()
    template <typename T>
    [[deprecated]] decltype(auto) null(T&& e)
    {
      return eq(std::forward<T>(e), symbols.intern("nil"));
    }

    // decltype(auto) list()
    auto list()
      -> const std::shared_ptr<cell>
    {
      return cell::nil;
    }

    template <typename T, typename... Ts>
    // decltype(auto) list(T&& head, Ts&&... tail)
    auto list(T&& head, Ts&&... tail)
      -> const std::shared_ptr<cell>
    {
      return cons(std::forward<T>(head), list(std::forward<Ts>(tail)...));
    }

    // TODO convert to cell::operator+()
    auto append(const std::shared_ptr<cell>& x, const std::shared_ptr<cell>& y)
      -> const std::shared_ptr<cell>
    {
      return null(x)
               ? y
               : cons(
                   car(x),
                   append(cdr(x), y)
                 );
    }

    auto zip(const std::shared_ptr<cell>& x, const std::shared_ptr<cell>& y)
      -> const std::shared_ptr<cell>
    {
      if (null(x) && null(y))
      {
        return symbols.intern("nil");
      }
      else if (!atom(x) && !atom(y))
      {
        return cons(
                 list(car(x), car(y)),
                 zip(cdr(x), cdr(y))
               );
      }
      else
      {
        return symbols.intern("nil");
      }
    }

    auto assoc(const std::shared_ptr<cell>& x, const std::shared_ptr<cell>& y)
      -> const std::shared_ptr<cell>
    {
      if (null(x))
      {
        return symbols.intern("nil");
      }
      else if (null(y))
      {
        return x;
      }
      else
      {
        return eq(caar(y), x) ? cadar(y) : assoc(x, cdr(y));
      }
    }

    auto evcon(const std::shared_ptr<cell>& c, const std::shared_ptr<cell>& a)
      -> const std::shared_ptr<cell>
    {
      return not eq(eval(caar(c), a), symbols.intern("nil"))
                   ? eval(cadar(c), a)
                   : evcon(cdr(c), a);
    }

    auto evlis(const std::shared_ptr<cell>& m, const std::shared_ptr<cell>& a)
      -> const std::shared_ptr<cell>
    {
      return null(m)
               ? symbols.intern("nil")
               : cons(eval(car(m), a), evlis(cdr(m), a));
    }
  } static eval {};
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_EVALUATOR_HPP

