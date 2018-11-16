#ifndef INCLUDED_MEEVAX_LISP_CELL_HPP
#define INCLUDED_MEEVAX_LISP_CELL_HPP

#include <memory>
#include <string>
#include <tuple>
#include <typeindex>
#include <typeinfo>
#include <unordered_map>
#include <utility>

#include <meevax/facade/identity.hpp>
#include <meevax/tuple/accessor.hpp>
#include <meevax/tuple/iterator.hpp>
#include <meevax/utility/type_erasure.hpp>

namespace meevax::lisp
{
  class cell;

  using cursor = tuple::iterator<cell>;

  std::unordered_map<std::string, cursor> symbols {
    std::make_pair("nil", cursor {nullptr})
  };

  struct cell
    : public std::tuple<cursor, cursor>,
      public facade::identity<cell>
  {
    template <typename... Ts>
    constexpr cell(Ts&&... args)
      : std::tuple<cursor, cursor> {std::forward<Ts>(args)...}
    {}

    virtual ~cell() = default;
  };
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_CELL_HPP

