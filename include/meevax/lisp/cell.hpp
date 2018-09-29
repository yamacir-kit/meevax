#ifndef INCLUDED_MEEVAX_LISP_CELL_HPP
#define INCLUDED_MEEVAX_LISP_CELL_HPP

#include <iostream>
#include <iterator>
#include <memory>
#include <string>
#include <typeindex>
#include <typeinfo>
#include <utility>

#include <boost/cstdlib.hpp>

#include <meevax/lisp/alias.hpp>
#include <meevax/lisp/error.hpp>
#include <meevax/utility/binder.hpp>
// #include <meevax/utility/recursive_iterator.hpp>

namespace meevax::lisp
{
  class cursor
    : public std::shared_ptr<cell>
  {
  public:
    using difference_type = std::ptrdiff_t;
    using value_type = cursor;
    using pointer = cursor;
    using reference = cursor&;
    using iterator_category = std::forward_iterator_tag;

    template <typename... Ts>
    constexpr cursor(Ts&&... args)
      : std::shared_ptr<cell> {std::forward<Ts>(args)...}
    {}

    cursor operator*() const noexcept;
    cursor operator++() noexcept;
  };

  // auto car = [](auto&& e) { return std::get<0>(*e); };
  // auto cdr = [](auto&& e) { return std::get<1>(*e); };
  //
  // using cursor = meevax::utility::recursive_iterator<cell, car, cdr>;

  cursor nil {nullptr};

  class cell
    : public std::pair<cursor, cursor>
  {
    friend class cursor;

  public:
    constexpr cell() = default;

    template <typename T>
    constexpr cell(T&& car)
      : std::pair<cursor, cursor> {std::forward<T>(car), nil}
    {}

    template <typename T, typename U>
    cell(T&& car, U&& cdr)
      : std::pair<cursor, cursor> {std::forward<T>(car), std::forward<U>(cdr)}
    {}

  public:
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

    if (e->type() == typeid(cell))
    {
      return os << "(" << *e << " . " << ++e << ")";
    }
    else if (e->type() == typeid(symbol))
    {
      return os << e->template as<symbol>();
    }
    else
    {
      throw std::runtime_error {std::to_string(__LINE__)};
    }
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

  inline cursor cursor::operator*() const noexcept
  {
    return get()->first;
  }

  inline cursor cursor::operator++() noexcept
  {
    return *this = get()->second;
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

