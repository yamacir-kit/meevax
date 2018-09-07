#ifndef INCLUDED_MEEVAX_UTILITY_TYPE_ERASURE_HPP
#define INCLUDED_MEEVAX_UTILITY_TYPE_ERASURE_HPP

#include <type_traits>
#include <typeinfo>
#include <utility>

#include <meevax/utility/conditionally_trivial_destructible.hpp>

namespace meevax::utility
{
  template <typename T, typename U>
  struct binder
    : public conditionally_trivial_destructible<T>,
      public conditionally_trivial_destructible<U>
  {
    using bound_type = conditionally_trivial_destructible<T>;

    explicit constexpr binder(const T& value)
      : bound_type {value}
    {}

    template <typename... Ts>
    explicit constexpr binder(Ts&&... xs)
      : bound_type {std::forward<Ts>(xs)...}
    {}

    auto type() const noexcept
      -> const std::type_info& override
    {
      return typeid(bound_type);
    }
  };
} // namespace meevex::utility

#endif // INCLUDED_MEEVAX_UTILITY_TYPE_ERASURE_HPP

