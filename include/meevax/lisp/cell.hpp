#ifndef INCLUDED_MEEVAX_LISP_CELL_HPP
#define INCLUDED_MEEVAX_LISP_CELL_HPP

#include <string>
#include <tuple>
#include <unordered_map>
#include <utility>

#include <meevax/facade/identity.hpp>
#include <meevax/tuple/iterator.hpp>

namespace meevax::lisp
{
  struct cell;

  using cursor = tuple::iterator<cell>;

  struct cell
    : public std::tuple<cursor, cursor>,
      public facade::identity<cell>
  {
    template <typename... Ts>
    constexpr cell(Ts&&... args)
      : std::tuple<cursor, cursor> {std::forward<Ts>(args)...}
    {}

    virtual ~cell() = default; // removable
  };

  const cursor nil {nullptr};

  std::unordered_map<std::string, cursor> symbols {std::make_pair("nil", nil)};
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_CELL_HPP

