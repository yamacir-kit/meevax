#ifndef INCLUDED_MEEVAX_UTILITY_DELAY_HPP
#define INCLUDED_MEEVAX_UTILITY_DELAY_HPP

#include <memory>
#include <type_traits>

#include <meevax/kernel/error.hpp>
#include <meevax/utility/requires.hpp>

namespace meevax
{
inline namespace utility
{
  template <typename F>
  struct delay
  {
    static inline F f {};

    // TODO SUPPORT UNARY FUNCTION!

    template <typename T, typename U, typename = void>
    struct viable
      : public std::false_type
    {};

    template <typename T, typename U>
    struct viable<T, U, std::void_t<decltype(std::invoke(std::declval<F>(), std::declval<T>(), std::declval<U>()))>>
      : public std::true_type
    {};

    template <typename T, typename U, typename = void>
    struct select
    {
      template <typename R>
      static auto apply(T&&, U&&) -> R
      {
        if constexpr (std::is_same<R, bool>::value)
        {
          return false;
        }
        else
        {
          throw error("no viable operation '", typeid(F).name(), " with ", typeid(T).name(), " and ", typeid(U).name());
        }
      }
    };

    template <typename T, typename U>
    struct select<T, U, typename std::enable_if<viable<T, U>::value>::type>
    {
      template <typename R>
      static constexpr auto apply(T&& x, U&& y) -> R
      {
        return f(std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
      }
    };

    template <typename R, typename... Ts>
    static constexpr auto yield(Ts&&... xs) -> decltype(auto)
    {
      return select<Ts...>().template apply<R>(std::forward<decltype(xs)>(xs)...);
    }
  };

  /* ---- Miscellaneous ----------------------------------------------------- */

  struct clone
  {
    template <typename T, REQUIRES(std::is_copy_constructible<T>)>
    auto operator ()(T const& origin, std::nullptr_t) const -> decltype(auto)
    {
      return std::make_shared<T>(origin);
    }
  };

  struct read
  {
    template <typename Port, typename... Ts>
    constexpr auto operator ()(Port&& port, Ts&&... xs) const -> decltype(auto)
    {
      return (port >> ... >> xs);
    }
  };

  struct write
  {
    template <typename Port, typename... Ts>
    constexpr auto operator ()(Port&& port, Ts&&... xs) const -> decltype(auto)
    {
      return (port << ... << xs);
    }
  };
} // namespace utility
} // namespace meevax

#endif // INCLUDED_MEEVAX_UTILITY_DELAY_HPP
