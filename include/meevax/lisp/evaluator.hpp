#ifndef INCLUDED_MEEVAX_LISP_EVALUATOR_HPP
#define INCLUDED_MEEVAX_LISP_EVALUATOR_HPP

#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>

#include <meevax/lisp/accessor.hpp>
#include <meevax/lisp/cell.hpp>
#include <meevax/lisp/error.hpp>
#include <meevax/lisp/function.hpp>

namespace meevax::lisp
{
inline namespace pure
{
  class evaluator
  {
    static inline std::unordered_map<std::string, const std::shared_ptr<cell>> s
    {
      {"atom",   cell::make_as<std::string>("atom")},
      {"car",    cell::make_as<std::string>("car")},
      {"cdr",    cell::make_as<std::string>("cdr")},
      {"cond",   cell::make_as<std::string>("cond")},
      {"cons",   cell::make_as<std::string>("cons")},
      {"define", cell::make_as<std::string>("define")},
      {"eq",     cell::make_as<std::string>("eq")},
      {"label",  cell::make_as<std::string>("label")},
      {"lambda", cell::make_as<std::string>("lambda")},
      {"quote",  cell::make_as<std::string>("quote")},
      {"true",   cell::make_as<std::string>("true")}
    };

    static inline auto env {cell::nil};

  public:
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
        if (eq(car(e), s["quote"]))
        {
          return cadr(e);
        }
        else if (eq(car(e), s["atom"]))
        {
          return atom(eval(cadr(e), a)) ? s["true"] : cell::nil;
        }
        else if (eq(car(e), s["eq"]))
        {
          return eq(eval(cadr(e), a), eval(caddr(e), a)) ? s["true"] : cell::nil;
        }
        else if (eq(car(e), s["cond"]))
        {
          return evcon(cdr(e), a);
        }
        else if (eq(car(e), s["car"]))
        {
          return car(eval(cadr(e), a));
        }
        else if (eq(car(e), s["cdr"]))
        {
          return cdr(eval(cadr(e), a));
        }
        else if (eq(car(e), s["cons"]))
        {
          return cons(eval(cadr(e), a), eval(caddr(e), a));
        }
        else if (eq(car(e), s["define"]))
        {
          env = cons(list(cadr(e), caddr(e)), env);
          return assoc(cadr(e), env);
        }
        else
        {
          return eval(cons(assoc(car(e), a), cdr(e)), a);
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
      return eq(eval(caar(c), a), s["true"]) ? eval(cadar(c), a) : evcon(cdr(c), a);
    }

    auto evlis(const std::shared_ptr<cell>& m, const std::shared_ptr<cell>& a)
      -> const std::shared_ptr<cell>
    {
      return null(m) ? cell::nil : cons(eval(car(m), a), evlis(cdr(m), a));
    }
  } static eval {};
} // namespace pure

namespace lexical_scoping_and_dynamic_toplevel_references
{
} // namespace lexical_scoping_and_dynamic_toplevel_references
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_EVALUATOR_HPP

