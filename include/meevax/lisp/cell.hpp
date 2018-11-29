#ifndef INCLUDED_MEEVAX_LISP_CELL_HPP
#define INCLUDED_MEEVAX_LISP_CELL_HPP

#include <memory>
#include <string>
#include <utility>

#include <meevax/facade/identity.hpp>
#include <meevax/lisp/iterator.hpp>
#include <meevax/utility/type_erasure.hpp>

namespace meevax::lisp
{
  struct pair; // forward decreation for type `cusror`

  // TODO using iterator = meevax::lisp::cursor<pair>;
  using cursor = iterator<pair>;
  const cursor nil {nullptr};

  struct pair
    : public std::pair<cursor, cursor>,
      public facade::identity<pair>
  {
    template <typename... Ts>
    constexpr pair(Ts&&... args)
      : std::pair<cursor, cursor> {std::forward<Ts>(args)...}
    {}

    virtual ~pair() = default; // removable
  };

  // Constructor interface like a traditional function.
  // using cons = pair;

  const cursor t {std::make_shared<
    utility::binder<std::string, pair>
  >("true")};
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_CELL_HPP

