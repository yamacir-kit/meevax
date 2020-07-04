#ifndef INCLUDED_MEEVAX_CONCEPTS_ADDABLE_HPP
#define INCLUDED_MEEVAX_CONCEPTS_ADDABLE_HPP

#include <type_traits>

namespace meevax::concepts
{
  template <typename T, typename U, typename = void>
  struct addable
    : public std::false_type
  {};

  template <typename T, typename U>
  struct addable<T, U, std::void_t<decltype(std::declval<T>() + std::declval<U>())>>
    : public std::true_type
  {};
} // namespace meevax::concepts

#endif // INCLUDED_MEEVAX_CONCEPTS_ADDABLE_HPP
