#ifndef INCLUDED_MEEVAX_CORE_PAIR_HPP
#define INCLUDED_MEEVAX_CORE_PAIR_HPP

#include <memory>
#include <string>
#include <utility>

#include <meevax/facade/identity.hpp>
#include <meevax/core/accessor.hpp>

namespace meevax::core
{
  // Forward decleation for struct cursor.
  struct pair;

  struct cursor
    : public accessor<pair>
  {
    template <typename... Ts>
    constexpr cursor(Ts&&... args)
      : accessor<pair> {std::forward<Ts>(args)...}
    {}
  };

  // This class must be constructed by std::make_shared<pair>.
  struct pair
    : public std::pair<cursor, cursor>,
      public facade::identity<pair>
  {
    template <typename... Ts>
    constexpr pair(Ts&&... args)
      : std::pair<cursor, cursor> {std::forward<Ts>(args)...}
    {}

    // NOTE Virtual destructor is removable if instantiate this type only via std::shared_ptr.
    virtual ~pair() = default;
  };

  decltype(auto) car(const cursor& exp) { return std::get<0>(exp.access()); }
  decltype(auto) cdr(const cursor& exp) { return std::get<1>(exp.access()); }

  // using cursor = accessor<pair>;
  const cursor nil {nullptr};

  const auto t {cursor::bind<std::string>("true")};
  const auto f {cursor::bind<std::string>("false")};
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_PAIR_HPP

