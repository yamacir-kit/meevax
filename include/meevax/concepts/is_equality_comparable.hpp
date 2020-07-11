#ifndef INCLUDED_MEEVAX_CONCEPTS_IS_EQUALITY_COMPARABLE_HPP
#define INCLUDED_MEEVAX_CONCEPTS_IS_EQUALITY_COMPARABLE_HPP

#include <type_traits>

namespace meevax::concepts
{
  template <typename T, typename = void>
  struct equality_comparable
    : public std::false_type
  {};

  template <typename T>
  struct equality_comparable<T, std::void_t<decltype(
           std::declval<T>() == std::declval<T>()
           )>>
    : public std::true_type
  {};

  template <typename T, typename U, typename = void>
  struct equality_comparable_with
    : public std::false_type
  {};

  template <typename T, typename U>
  struct equality_comparable_with<T, U, std::void_t<decltype(
           std::declval<T>() == std::declval<U>()
           )>>
    : public std::true_type
  {};

  template <typename T, typename U, typename = void>
  struct not_equality_comparable_with
    : public std::false_type
  {};

  template <typename T, typename U>
  struct not_equality_comparable_with<T, U, std::void_t<decltype(
           std::declval<T>() != std::declval<U>()
           )>>
    : public std::true_type
  {};
} // namespace meevax::concepts

#endif // INCLUDED_MEEVAX_CONCEPTS_IS_EQUALITY_COMPARABLE_HPP

