#ifndef INCLUDED_MEEVAX_FACADE_CONDITIONALLY_TRIVIAL_DESTRUCTIBLE_HPP
#define INCLUDED_MEEVAX_FACADE_CONDITIONALLY_TRIVIAL_DESTRUCTIBLE_HPP

#include <type_traits>
#include <utility>

namespace meevax::facade
{
  template <typename T, bool IsTriviallyDestructible = std::is_trivially_destructible<T>::value>
  struct conditionally_trivial_destructible
    : public T
  {
    template <typename... Ts>
    explicit constexpr conditionally_trivial_destructible(Ts&&... xs)
      : T {std::forward<Ts>(xs)...}
    {}

    ~conditionally_trivial_destructible() = default;
  };

  template <typename T>
  struct conditionally_trivial_destructible<T, false>
    : public T
  {
    template <typename... Ts>
    explicit constexpr conditionally_trivial_destructible(Ts&&... xs)
      : T {std::forward<Ts>(xs)...}
    {}

    virtual ~conditionally_trivial_destructible()
    {
      static_cast<T&>(*this).~T();
    }
  };
} // namespace meevex::utility

#endif // INCLUDED_MEEVAX_FACADE_CONDITIONALLY_TRIVIAL_DESTRUCTIBLE_HPP

