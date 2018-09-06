#ifndef INCLUDED_MEEVAX_UTILITY_TYPE_ERASURE_HPP
#define INCLUDED_MEEVAX_UTILITY_TYPE_ERASURE_HPP

#include <type_traits>
#include <utility>

#include <meevax/utility/conditionally_trivial_destructible.hpp>

namespace meevax::utility
{
  template <typename T, typename U>
  struct binder
    : public conditionally_trivial_destructible<T>,
      public conditionally_trivial_destructible<U>
  {
    using erasure     = conditionally_trivial_destructible<T>;
    using common_base = conditionally_trivial_destructible<U>;

    explicit constexpr binder(const T& value)
      : erasure {value}
    {}

    template <typename... Ts>
    explicit constexpr binder(Ts&&... xs)
      : erasure {std::forward<Ts>(xs)...}
    {}

    auto type() const noexcept
      -> const std::type_info& override
    {
      return typeid(erasure);
    }
  };
} // namespace meevex::utility

#endif // INCLUDED_MEEVAX_UTILITY_TYPE_ERASURE_HPP

