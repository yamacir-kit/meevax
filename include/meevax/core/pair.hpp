#ifndef INCLUDED_MEEVAX_CORE_PAIR_HPP
#define INCLUDED_MEEVAX_CORE_PAIR_HPP

#include <memory>
#include <string>
#include <utility>

#include <meevax/facade/identity.hpp>
#include <meevax/core/accessor.hpp>

namespace meevax::core
{
  // This class must be constructed by std::make_shared<pair>.
  struct pair
    : public std::pair<accessor<pair>, accessor<pair>>,
      public facade::identity<pair>
  {
    template <typename... Ts>
    constexpr pair(Ts&&... args)
      : std::pair<accessor<pair>, accessor<pair>> {std::forward<Ts>(args)...}
    {}

    // NOTE Virtual destructor is removable if instanciate this type only via std::shared_ptr.
    virtual ~pair() = default;
  };

  using cursor = accessor<pair>;
  const cursor nil {nullptr};

  const auto t {cursor::bind<std::string>("true")};
  const auto f {cursor::bind<std::string>("false")};
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_PAIR_HPP

