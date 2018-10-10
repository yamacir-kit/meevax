#ifndef INCLUDED_MEEVAX_FACADE_CONDITIONALLY_TRIVIAL_DESTRUCTIBLE_HPP
#define INCLUDED_MEEVAX_FACADE_CONDITIONALLY_TRIVIAL_DESTRUCTIBLE_HPP

#include <type_traits>
#include <utility>

namespace meevax::facade
{
  template <typename T, bool Cond = std::is_trivially_destructible<T>::value>
  struct conditionally_trivial_destructible
    : public T,
      public std::true_type
  {
    template <typename... Ts>
    explicit constexpr conditionally_trivial_destructible(Ts&&... args)
      : T {std::forward<Ts>(args)...}
    {}

    ~conditionally_trivial_destructible() = default;
  };

  template <typename T>
  struct conditionally_trivial_destructible<T, false>
    : public T,
      public std::false_type
  {
    template <typename... Ts>
    explicit constexpr conditionally_trivial_destructible(Ts&&... args)
      : T {std::forward<Ts>(args)...}
    {}

    virtual ~conditionally_trivial_destructible()
    {
      static_cast<T&>(*this).~T();
    }
  };
} // namespace meevex::utility

#endif // INCLUDED_MEEVAX_FACADE_CONDITIONALLY_TRIVIAL_DESTRUCTIBLE_HPP

