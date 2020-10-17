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

  #define macroexpand(OPERATION)                                               \
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

  macroexpand(addition);
  macroexpand(subtraction);
  macroexpand(multiplication);
  macroexpand(division);
  macroexpand(modulo);

  macroexpand(equal_to);
  macroexpand(not_equal_to);
  macroexpand(less_than);
  macroexpand(less_than_or_equal_to);
  macroexpand(greater_than);
  macroexpand(greater_than_or_equal_to);

  #undef macroexpand

  /* ---- Unary Operations -------------------------------------------------- */

  #define macroexpand(OPERATION)                                               \
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

  macroexpand(unary_plus);
  macroexpand(unary_minus);

  #undef macroexpand

  /* ---- Lazy Operations --------------------------------------------------- */

  #define macroexpand(OPERATION)                                               \
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

  macroexpand(addition);
  macroexpand(subtraction);
  macroexpand(multiplication);
  macroexpand(division);
  macroexpand(modulo);

  #undef macroexpand
}} // namespace meevax::type_traits

#endif // INCLUDED_MEEVAX_TYPE_TRAITS_OPERATION_SUPPORT_HPP
