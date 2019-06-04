#ifndef INCLUDED_MEEVAX_CONCEPTS_IS_EQUALITY_COMPARABLE_HPP
#define INCLUDED_MEEVAX_CONCEPTS_IS_EQUALITY_COMPARABLE_HPP

#include <type_traits>

namespace meevax::concepts
{
  template <typename T, typename = void>
  struct is_equality_comparable
    : public std::false_type
  {};

  template <typename T>
  struct is_equality_comparable<
           T,
           std::void_t<decltype(
             std::declval<const T&>() == std::declval<const T&>()
           )>
         >
    : public std::true_type
  {};
} // namespace meevax::concepts

#endif // INCLUDED_MEEVAX_CONCEPTS_IS_EQUALITY_COMPARABLE_HPP

