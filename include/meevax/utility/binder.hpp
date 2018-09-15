#ifndef INCLUDED_MEEVAX_UTILITY_BINDER_HPP
#define INCLUDED_MEEVAX_UTILITY_BINDER_HPP

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
    explicit constexpr binder(const T& value)
      : conditionally_trivial_destructible<T> {value}
    {}

    template <typename... Ts>
    explicit constexpr binder(Ts&&... xs)
      : conditionally_trivial_destructible<T> {std::forward<Ts>(xs)...}
    {}

    auto type() const noexcept
      -> const std::type_info& override
    {
      return typeid(T);
    }
  };
} // namespace meevex::utility

#endif // INCLUDED_MEEVAX_UTILITY_BINDER_HPP

