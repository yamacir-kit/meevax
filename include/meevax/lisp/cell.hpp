#ifndef INCLUDED_MEEVAX_LISP_CELL_HPP
#define INCLUDED_MEEVAX_LISP_CELL_HPP

#include <iostream>
#include <memory>
#include <string>
#include <typeindex>
#include <typeinfo>
#include <utility>

#include <boost/cstdlib.hpp>

#include <meevax/lisp/error.hpp>
#include <meevax/utility/binder.hpp>
#include <meevax/utility/recursive_tuple_iterator.hpp>

namespace meevax::lisp
{
  class cell;

  using cursor = utility::recursive_tuple_iterator<cell>;
  cursor nil {nullptr};

  using symbol = const std::string;

  struct cell
    : public std::pair<cursor, cursor>
  {
    constexpr cell() = default;

    template <typename T>
    constexpr cell(T&& car)
      : std::pair<cursor, cursor> {std::forward<T>(car), nil}
    {}

    template <typename T, typename U>
    cell(T&& car, U&& cdr)
      : std::pair<cursor, cursor> {std::forward<T>(car), std::forward<U>(cdr)}
    {}

    template <typename T>
    auto as() const try
    {
      return dynamic_cast<const T&>(*this);
    }
    catch (const std::bad_cast& error)
    {
      std::cerr << error("arbitrary dispatch failed for (" << first << " . " << second << ")") << std::endl;
      std::exit(boost::exit_exception_failure);
    }

    virtual auto type() const noexcept
      -> const std::type_info&
    {
      return typeid(cell);
    }
  };

  std::ostream& operator<<(std::ostream& os, cursor e)
  {
    if (!e)
    {
      return os << "nil";
    }

    if (e->type() == typeid(symbol))
    {
      return os << e->template as<symbol>();
    }

    for (os << '(' << *e; ++e; os << ' ' << *e)
    {
      if (e->type() != typeid(cell)) // is not pure list
      {
        return os << " . " << e << ')';
      }
    }

    return os << ')';
  }

  template <typename T>
  bool atom(T&& e) try
  {
    static const std::unordered_map<std::type_index, bool> dispatch
    {
      {typeid(cell), false},
      {typeid(symbol), true}
    };

    return !e || dispatch.at(e->type());
  }
  catch (const std::out_of_range& error)
  {
    std::cerr << error("atom dispatch failed for " << e) << std::endl;
    std::exit(boost::exit_exception_failure);
  }

  template <typename T, typename... Ts>
  cursor make_as(Ts&&... args)
  {
    using binder = meevax::utility::binder<T, cell>;
    return std::make_shared<binder>(std::forward<Ts>(args)...);
  }

  auto cons = [](auto&&... args) -> cursor
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

