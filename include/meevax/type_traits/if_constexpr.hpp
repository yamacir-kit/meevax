#ifndef INCLUDED_MEEVAX_TYPE_TRAITS_IF_CONSTEXPR_HPP
#define INCLUDED_MEEVAX_TYPE_TRAITS_IF_CONSTEXPR_HPP

#include <sstream>
#include <stdexcept>
#include <type_traits>

#include <meevax/concepts/is_equality_comparable.hpp>

namespace meevax { inline namespace type_traits
{
  #define BOILERPLATE(NAMESPACE, TRAIT)                                        \
  template <typename T, typename = void>                                       \
  struct if_##TRAIT                                                            \
  {                                                                            \
    template <typename R, typename... Ts>                                      \
    static auto invoke(Ts&&...) -> R                                           \
    {                                                                          \
      std::stringstream port {};                                               \
      port << typeid(T).name() << " is not " #TRAIT;                           \
      throw std::logic_error { port.str() };                                   \
    }                                                                          \
  };                                                                           \
                                                                               \
  template <typename T>                                                        \
  struct if_##TRAIT<T, typename std::enable_if<NAMESPACE::TRAIT<T>::value>::type> \
  {                                                                            \
    template <typename R, typename F, typename... Ts>                          \
    static auto invoke(F&& f, Ts&&... xs) -> R                                 \
    {                                                                          \
      return f(std::forward<decltype(xs)>(xs)...);                             \
    }                                                                          \
  }

  BOILERPLATE(std, is_copy_constructible);

  BOILERPLATE(concepts, equality_comparable);

  #undef BOILERPLATE

  #define BOILERPLATE(NAMESPACE, TRAIT)                                        \
  template <typename T, typename U, typename = void>                           \
  struct if_##TRAIT                                                            \
  {                                                                            \
    template <typename R, typename... Ts>                                      \
    static auto invoke(Ts&&...) -> R                                           \
    {                                                                          \
      std::stringstream port {};                                               \
      port << typeid(T).name() << " and "                                      \
           << typeid(U).name() << " are not " #TRAIT;                          \
      throw std::logic_error { port.str() };                                   \
    }                                                                          \
  };                                                                           \
                                                                               \
  template <typename T, typename U>                                            \
  struct if_##TRAIT<T, U, typename std::enable_if<NAMESPACE::TRAIT<T, U>::value>::type> \
  {                                                                            \
    template <typename R, typename F, typename... Ts>                          \
    static auto invoke(F&& f, Ts&&... xs) -> R                                 \
    {                                                                          \
      return f(std::forward<decltype(xs)>(xs)...);                             \
    }                                                                          \
  }

  // BOILERPLATE(concepts, addable);
  // BOILERPLATE(concepts, divisible);
  // BOILERPLATE(concepts, multipliable);
  // BOILERPLATE(concepts, subtractable);
  // BOILERPLATE(concepts, supports_modulo_operation);

  BOILERPLATE(concepts, equality_comparable_with);
  BOILERPLATE(concepts, not_equality_comparable_with);
  BOILERPLATE(concepts, greater_equal_comparable);
  BOILERPLATE(concepts, greater_than_comparable);
  BOILERPLATE(concepts, less_equal_comparable);
  BOILERPLATE(concepts, less_than_comparable);

  #undef BOILERPLATE
}} // namespace meevax::type_traits

#endif // INCLUDED_MEEVAX_TYPE_TRAITS_IF_CONSTEXPR_HPP
