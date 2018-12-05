#ifndef INCLUDED_MEEVAX_LISP_PAIR_HPP
#define INCLUDED_MEEVAX_LISP_PAIR_HPP

#include <memory>
#include <string>
#include <utility>

#include <meevax/facade/identity.hpp>
#include <meevax/lisp/iterator.hpp>
#include <meevax/utility/type_erasure.hpp>

namespace meevax::lisp
{
  struct pair; // forward decreation for type `cusror`

  using cursor = iterator<pair>;
  const cursor nil {nullptr};

  // This class must be constructed by std::make_shared<pair>.
  struct pair
    : public std::pair<cursor, cursor>,
      public facade::identity<pair>
  {
    template <typename... Ts>
    constexpr pair(Ts&&... args)
      : std::pair<cursor, cursor> {std::forward<Ts>(args)...}
    {}

    // NOTE Virtual destructor is removable if instanciate this type only via std::shared_ptr.
    virtual ~pair() = default;
  };

  const cursor t {std::make_shared<
    utility::binder<std::string, pair>
  >("true")};
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_PAIR_HPP

