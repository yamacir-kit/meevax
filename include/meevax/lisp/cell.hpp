#ifndef INCLUDED_MEEVAX_LISP_CELL_HPP
#define INCLUDED_MEEVAX_LISP_CELL_HPP

#include <memory>
#include <string>
#include <tuple>
#include <unordered_map>
#include <utility>

#include <meevax/facade/identity.hpp>
#include <meevax/tuple/iterator.hpp>
#include <meevax/utility/type_erasure.hpp>

namespace meevax::lisp
{
  struct cell; // forward decreation for type `cusror`
  using cursor = tuple::iterator<std::shared_ptr<cell>>;

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

  const cursor t {std::make_shared<
    utility::binder<std::string, cell>
  >("true")};

  std::unordered_map<std::string, cursor> symbols {};
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_CELL_HPP

