#ifndef INCLUDED_MEEVAX_LISP_CELL_HPP
#define INCLUDED_MEEVAX_LISP_CELL_HPP

#include <memory>
#include <string>
#include <tuple>
#include <utility>

#include <meevax/facade/identity.hpp>
#include <meevax/lisp/iterator.hpp>
#include <meevax/utility/type_erasure.hpp>

namespace meevax::lisp
{
  struct cell; // forward decreation for type `cusror`

  using cursor = iterator<cell>;
  const cursor nil {nullptr};

  struct cell
    : public std::pair<cursor, cursor>,
      public facade::identity<cell>
  {
    template <typename... Ts>
    constexpr cell(Ts&&... args)
      : std::pair<cursor, cursor> {std::forward<Ts>(args)...}
    {}

    virtual ~cell() = default; // removable
  };

  const cursor t {std::make_shared<
    utility::binder<std::string, cell>
  >("true")};
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_CELL_HPP

