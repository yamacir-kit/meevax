#ifndef INCLUDED_MEEVAX_TYPE_TRAITS_IS_DEREFERENCEABLE_HPP
#define INCLUDED_MEEVAX_TYPE_TRAITS_IS_DEREFERENCEABLE_HPP

#include <type_traits>

namespace meevax
{
inline namespace type_traits
{
  template <typename T, typename = void>
  struct is_dereferenceable
    : public std::false_type
  {};

  template <typename T>
  struct is_dereferenceable<T, std::void_t<decltype(*std::declval<T>())>>
    : public std::true_type
  {};
} // namespace type_traits
} // namespace meevax

#endif // INCLUDED_MEEVAX_TYPE_TRAITS_IS_DEREFERENCEABLE_HPP
