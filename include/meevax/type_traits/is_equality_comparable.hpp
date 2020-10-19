#ifndef INCLUDED_MEEVAX_TYPE_TRAITS_IS_EQUALITY_COMPARABLE_HPP
#define INCLUDED_MEEVAX_TYPE_TRAITS_IS_EQUALITY_COMPARABLE_HPP

#include <type_traits>

namespace meevax { inline namespace type_traits
{
  template <typename T, typename = void>
  struct is_equality_comparable
    : public std::false_type
  {};

  template <typename T>
  struct is_equality_comparable<T, std::void_t<decltype(std::declval<T>() == std::declval<T>())>>
    : public std::true_type
  {};

  template <typename T, typename U, typename = void>
  struct is_equality_comparable_with
    : public std::false_type
  {};

  template <typename T, typename U>
  struct is_equality_comparable_with<T, U, std::void_t<decltype(std::declval<T>() == std::declval<U>())>>
    : public std::true_type
  {};
}} // namespace meevax::type_traits

#endif // INCLUDED_MEEVAX_TYPE_TRAITS_IS_EQUALITY_COMPARABLE_HPP
