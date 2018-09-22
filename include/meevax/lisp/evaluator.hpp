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

namespace meevax::lisp
{
  class evaluator
  {
    static inline auto env {nil};

  public:
    evaluator()
    {
      define("quote", [](const auto& e, const auto&)
      {
        return cadr(e);
      });

      define("atom", [&](const auto& e, const auto& a)
      {
        return atom(eval(cadr(e), a)) ? symbols.intern("true") : nil;
      });

      define("eq", [&](const auto& e, const auto& a)
      {
        return eval(cadr(e), a) == eval(caddr(e), a) ? symbols.intern("true") : nil;
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
        return eval(cadr(e), a) + eval(caddr(e), a);
      });

      define("define", [&](const auto& e, const auto& a)
      {
        env = list(cadr(e), caddr(e)) + env;
        return assoc(cadr(e), a);
      });
    }

    decltype(auto) operator()(const std::shared_ptr<cell>& e)
    {
      return eval(e, env);
    }

    template <typename F>
    void define(const std::string& s, F&& proc)
    {
      env = list(symbols.intern(s), cell::make_as<special>(proc)) + env;
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
        if (auto proc {assoc(car(e), a)}; proc->type() == typeid(special))
        {
          return proc->as<special>()(e, a);
        }
        else if (atom(proc))
        {
          std::cerr << error("using atom \"" << proc << "\" as procedure") << std::endl;
          return nil;
        }
        else
        {
          return eval(proc + cdr(e), a);
        }
      }
      else if (caar(e) == symbols.intern("label"))
      {
        return eval(caddar(e) + cdr(e), list(cadar(e), car(e)) + a);
      }
      else if (caar(e) == symbols.intern("lambda"))
      {
        return eval(caddar(e), append(zip(cadar(e), evlis(cdr(e), a)), a));
      }
      else
      {
        std::cerr << error("unknown function \"" << car(e) << "\"") << std::endl;
        return nil;
      }
    }

  private:
    // TODO convert to cell::operator bool()
    template <typename T>
    [[deprecated]] decltype(auto) null(T&& e)
    {
      return e == nil;
    }

    template <typename... Ts>
    auto list(Ts&&... xs)
      -> const std::shared_ptr<cell>
    {
      return (xs + ... + nil);
    }

    // TODO convert to cell::operator+()
    template <typename T, typename U>
    auto append(T&& x, U&& y)
      -> const std::shared_ptr<cell>
    {
      return x == nil ? y : car(x) + append(cdr(x), y);
    }

    template <typename T, typename U>
    auto zip(T&& x, U&& y)
      -> const std::shared_ptr<cell>
    {
      if (x == nil && y == nil)
      {
        return nil;
      }
      else if (!atom(x) && !atom(y))
      {
        return list(car(x), car(y)) + zip(cdr(x), cdr(y));
      }
      else
      {
        return nil;
      }
    }

    auto assoc(const std::shared_ptr<cell>& x, const std::shared_ptr<cell>& y)
      -> const std::shared_ptr<cell>
    {
      if (x == nil)
      {
        return nil;
      }
      else if (y == nil)
      {
        return x;
      }
      else
      {
        return caar(y) == x ? cadar(y) : assoc(x, cdr(y));
      }
    }

    auto evcon(const std::shared_ptr<cell>& c, const std::shared_ptr<cell>& a)
      -> const std::shared_ptr<cell>
    {
      return eval(caar(c), a) != nil ? eval(cadar(c), a) : evcon(cdr(c), a);
    }

    auto evlis(const std::shared_ptr<cell>& m, const std::shared_ptr<cell>& a)
      -> const std::shared_ptr<cell>
    {
      return m == nil ? nil : eval(car(m), a) + evlis(cdr(m), a);
    }
  } static eval {};
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_EVALUATOR_HPP

