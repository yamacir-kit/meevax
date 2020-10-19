#ifndef INCLUDED_MEEVAX_UTILITY_DELAY_HPP
#define INCLUDED_MEEVAX_UTILITY_DELAY_HPP

#include <sstream> // TODO #include <meevax/kernel/error.hpp>
#include <stdexcept>
#include <type_traits>
#include <utility>

namespace meevax { inline namespace utility
{
  template <typename F>
  struct delay
  {
    static inline F f {};

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
        std::stringstream port {};
        port << typeid(T).name() << " and "
             << typeid(U).name() << " are not supports operation "
             << typeid(F).name();
        throw std::runtime_error(port.str());
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
}} // namespace meevax::utility

#endif // INCLUDED_MEEVAX_UTILITY_DELAY_HPP
