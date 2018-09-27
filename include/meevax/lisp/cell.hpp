#ifndef INCLUDED_MEEVAX_LISP_CELL_HPP
#define INCLUDED_MEEVAX_LISP_CELL_HPP

#include <iostream>
#include <memory>
#include <string>
#include <typeindex>
#include <typeinfo>
#include <utility>

#include <boost/cstdlib.hpp>

#include <meevax/lisp/alias.hpp>
#include <meevax/lisp/error.hpp>
#include <meevax/utility/binder.hpp>

namespace meevax::lisp
{
  cursor nil {nullptr};

  class cell
  {
    cursor car_, cdr_;

  public:
    explicit constexpr cell() noexcept = default;

    template <typename T>
    explicit cell(T&& car)
      : car_ {std::forward<T>(car)}
    {}

    template <typename T, typename U>
    explicit cell(T&& car, U&& cdr)
      : car_ {std::forward<T>(car)},
        cdr_ {std::forward<U>(cdr)}
    {}

  public:
    template <typename T, typename... Ts>
    static cursor make_as(Ts&&... xs)
    {
      return std::make_shared<
               meevax::utility::binder<T, cell>
             >(std::forward<Ts>(xs)...);
    }

    template <typename T>
    auto as() const noexcept try
    {
      return dynamic_cast<const T&>(*this);
    }
    catch (const std::bad_cast& error)
    {
      std::cerr << error("arbitrary dispatch failed for (" << car_ << " . " << cdr_ << ")") << std::endl;
      std::exit(boost::exit_exception_failure);
    }

    virtual auto type() const noexcept
      -> const std::type_info&
    {
      return typeid(cell);
    }

  public:
    template <typename T>
    friend bool atom(T&& e) try
    {
      static const std::unordered_map<std::type_index, bool> dispatch
      {
        {typeid(cell), false},
        {typeid(symbol), true}
      };

      return !e or (!e->cdr_ and dispatch.at(e->type()));
    }
    catch (const std::out_of_range& error)
    {
      std::cerr << error("boolean dispatch failed for " << e) << std::endl;
      std::exit(boost::exit_exception_failure);
    }

    // TODO operator*
    template <typename T>
    friend decltype(auto) car(T&& e) noexcept
    {
      return (e && e->car_) ? e->car_ : nil;
    }

    // TODO operator++
    template <typename T>
    friend decltype(auto) cdr(T&& e) noexcept
    {
      return (e && e->cdr_) ? e->cdr_ : nil;
    }

  public:
    friend auto operator<<(std::ostream& os, cursor& e)
      -> decltype(os)
    {
      if (!e)
      {
        return os << "nil";
      }

      if (e->type() == typeid(cell))
      {
        return os << "(" << e->car_ << " . " << e->cdr_ << ")";
      }
      else if (e->type() == typeid(symbol))
      {
        return os << e->as<symbol>();
      }
      else
      {
        throw std::runtime_error {std::to_string(__LINE__)};
      }
    }
  };

  // cursor nil {cell::make_as<symbol>("nil")};

  template <auto N, typename Sexp>
  decltype(auto) cdr(Sexp&& e) noexcept
  {
    if constexpr (N)
    {
      return cdr<N-1>(cdr(e));
    }
    else
    {
      return e;
    }
  }

  template <auto N, auto... Ns, typename Sexp>
  decltype(auto) car(Sexp&& e) noexcept
  {
    if constexpr (sizeof...(Ns))
    {
      return car<Ns...>(car(cdr<N>(e)));
    }
    else
    {
      return car(cdr<N>(e));
    }
  }

  auto cons = [](auto&&... args)
  {
    return std::make_shared<cell>(std::forward<decltype(args)>(args)...);
  };

  template <typename T, typename U>
  decltype(auto) operator|(T&& head, U&& tail)
  {
    return cons(std::forward<T>(head), std::forward<U>(tail));
  }
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_CELL_HPP

