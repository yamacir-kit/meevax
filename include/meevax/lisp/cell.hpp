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
  extern const std::shared_ptr<cell> nil;

  class cell
  {
    const std::shared_ptr<cell> car_, cdr_;

  public:
    explicit cell() noexcept = default;

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
    static auto make_as(Ts&&... xs)
      -> const std::shared_ptr<cell>
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
      std::cerr << error("dynamic_cast failed for \"" << type().name() << "\"") << std::endl;
      std::exit(boost::exit_exception_failure);
    }

    virtual auto type() const noexcept
      -> const std::type_info&
    {
      return typeid(cell);
    }

  public:
    friend bool atom(const std::shared_ptr<cell>& e)
    {
      static const std::unordered_map<std::type_index, bool> dispatcher
      {
        {typeid(symbol), true}
      };

      return !e->cdr_ && dispatcher.at(e->type());
    }

    template <typename T>
    friend decltype(auto) car(T&& e) noexcept
    {
      return (e && e->car_) ? e->car_ : nil;
    }

    template <typename T>
    friend decltype(auto) cdr(T&& e) noexcept
    {
      return (e && e->cdr_) ? e->cdr_ : nil;
    }

  public:
    friend auto operator<<(std::ostream& os, const std::shared_ptr<cell>& e)
      -> decltype(os)
    {
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

  const std::shared_ptr<cell> nil {cell::make_as<symbol>("nil")};

  template <typename... Ts>
  decltype(auto) cons(Ts&&... xs)
  {
    return std::make_shared<cell>(std::forward<Ts>(xs)...);
  }

  template <typename T, typename U>
  decltype(auto) operator|(T&& head, U&& tail)
  {
    return cons(head, tail);
  }
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_CELL_HPP

