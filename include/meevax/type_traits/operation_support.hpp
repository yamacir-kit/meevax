#ifndef INCLUDED_MEEVAX_TYPE_TRAITS_OPERATION_SUPPORT_HPP
#define INCLUDED_MEEVAX_TYPE_TRAITS_OPERATION_SUPPORT_HPP

#include <sstream>
#include <stdexcept>
#include <type_traits>

#include <meevax/functional/operation.hpp>
#include <utility>

namespace meevax { inline namespace type_traits
{
  /* ---- Binary Operations ------------------------------------------------- */

  #define BOILERPLATE(OPERATION)                                               \
  template <typename T, typename U, typename = void>                           \
  struct supports_##OPERATION##_operation                                      \
    : public std::false_type                                                   \
  {};                                                                          \
                                                                               \
  template <typename T, typename U>                                            \
  struct supports_##OPERATION##_operation<T, U,                                \
    std::void_t<decltype(                                                      \
      std::invoke(OPERATION(), std::declval<const T&>(), std::declval<const U&>()))>> \
    : public std::true_type                                                    \
  {}

  BOILERPLATE(addition);
  BOILERPLATE(subtraction);
  BOILERPLATE(multiplication);
  BOILERPLATE(division);
  BOILERPLATE(modulo);

  BOILERPLATE(equal_to);
  BOILERPLATE(not_equal_to);
  BOILERPLATE(less_than);
  BOILERPLATE(less_than_or_equal_to);
  BOILERPLATE(greater_than);
  BOILERPLATE(greater_than_or_equal_to);

  BOILERPLATE(read);
  BOILERPLATE(write);

  #undef BOILERPLATE

  /* ---- Unary Operations -------------------------------------------------- */

  #define BOILERPLATE(OPERATION)                                               \
  template <typename T, typename = void>                                       \
  struct supports_##OPERATION##_operation                                      \
    : public std::false_type                                                   \
  {};                                                                          \
                                                                               \
  template <typename T>                                                        \
  struct supports_##OPERATION##_operation<T,                                   \
    std::void_t<decltype(                                                      \
      std::invoke(OPERATION(), std::declval<const T&>()))>>                    \
    : public std::true_type                                                    \
  {}

  BOILERPLATE(unary_plus);
  BOILERPLATE(unary_minus);

  BOILERPLATE(prefix_increment);
  BOILERPLATE(prefix_decrement);
  BOILERPLATE(postfix_increment);
  BOILERPLATE(postfix_decrement);

  #undef BOILERPLATE

  /* ---- Lazy Operation Applications ----------------------------------------*/

  #define BOILERPLATE(OPERATION)                                               \
  template <typename T, typename U, typename = void>                           \
  struct if_supports_##OPERATION##_operation                                   \
  {                                                                            \
    template <typename R, typename... Ts>                                      \
    static auto apply(Ts&&...) -> R                                            \
    {                                                                          \
      std::stringstream port {};                                               \
      port << typeid(T).name() << " and "                                      \
           << typeid(U).name()                                                 \
           << " are not supports " #OPERATION " operation";                    \
      throw std::runtime_error(port.str());                                    \
    }                                                                          \
  };                                                                           \
                                                                               \
  template <typename T, typename U>                                            \
  struct if_supports_##OPERATION##_operation<T, U,                             \
    typename std::enable_if<                                                   \
      supports_##OPERATION##_operation<T, U>::value                            \
    >::type>                                                                   \
  {                                                                            \
    template <typename R, typename... Ts>                                      \
    static constexpr auto apply(Ts&&... xs) -> R                               \
    {                                                                          \
      return std::invoke(OPERATION(), std::forward<decltype(xs)>(xs)...);      \
    }                                                                          \
  };                                                                           \
                                                                               \
  template <typename R, typename... Ts>                                        \
  constexpr auto apply_if_supports_##OPERATION##_operation(Ts&&... xs)         \
    -> decltype(auto)                                                          \
  {                                                                            \
    return if_supports_##OPERATION##_operation<Ts...>::template apply<R>(      \
      std::forward<decltype(xs)>(xs)...);                                      \
  } static_assert(true)

  BOILERPLATE(addition);
  BOILERPLATE(subtraction);
  BOILERPLATE(multiplication);
  BOILERPLATE(division);
  BOILERPLATE(modulo);

  BOILERPLATE(equal_to);
  BOILERPLATE(not_equal_to);
  BOILERPLATE(less_than);
  BOILERPLATE(less_than_or_equal_to);
  BOILERPLATE(greater_than);
  BOILERPLATE(greater_than_or_equal_to);

  BOILERPLATE(unary_plus);
  BOILERPLATE(unary_minus);

  BOILERPLATE(prefix_increment);
  BOILERPLATE(prefix_decrement);
  BOILERPLATE(postfix_increment);
  BOILERPLATE(postfix_decrement);

  #undef BOILERPLATE

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
}} // namespace meevax::type_traits

#endif // INCLUDED_MEEVAX_TYPE_TRAITS_OPERATION_SUPPORT_HPP
