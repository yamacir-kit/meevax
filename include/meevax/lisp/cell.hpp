#ifndef INCLUDED_MEEVAX_LISP_CELL_HPP
#define INCLUDED_MEEVAX_LISP_CELL_HPP

#include <memory>
#include <string>
#include <tuple>
#include <typeindex>
#include <typeinfo>
#include <utility>

#include <meevax/facade/identity.hpp>
#include <meevax/tuple/accessor.hpp>
#include <meevax/tuple/iterator.hpp>
#include <meevax/utility/heterogeneous_dictionary.hpp>
// #include <meevax/utility/recursive_binary_tuple_iterator.hpp>
#include <meevax/utility/type_erasure.hpp>

#define caar(e) car(car(e))
#define cadar(e) car(cdr(car(e)))
#define caddar(e) car(cdr(cdr(car(e))))

#define cadr(e) car(cdr(e))
#define caddr(e) car(cdr(cdr(e)))
#define cadddr(e) car(cdr(cdr(cdr(e))))

namespace meevax::lisp
{
  class cell;

  using cursor = tuple::iterator<cell>;

  tuple::accessor<0> car {};
  tuple::accessor<1> cdr {};

  template <typename T>
  struct bind
  {
    template <typename... Ts>
    cursor operator()(Ts&&... args)
    {
      using binder = utility::binder<T, cell>;
      return std::make_shared<binder>(std::forward<Ts>(args)...);
    }
  };

  utility::heterogeneous_dictionary<cursor, bind<std::string>> symbols {
    std::make_pair("nil", cursor {nullptr})
  };

  struct cell
    : public std::tuple<cursor, cursor>,
      public facade::identity<cell>
  {
    template <typename T>
    constexpr cell(T&& head)
      : std::tuple<cursor, cursor> {std::forward<T>(head), symbols.intern("nil")}
    {}

    template <typename... Ts>
    constexpr cell(Ts&&... args)
      : std::tuple<cursor, cursor> {std::forward<Ts>(args)...}
    {}

    virtual ~cell() = default;
  };

  template <typename T>
  bool atom(T&& e)
  {
    static const std::unordered_map<std::type_index, bool> dispatch
    {
      {typeid(cell), false},
      {typeid(std::string), true}
    };

    return !e || dispatch.at(e->type());
  }
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_CELL_HPP

