#ifndef MEEVAX_TYPE_TRAITS_IS_STREAM_INSERTABLE_HPP
#define MEEVAX_TYPE_TRAITS_IS_STREAM_INSERTABLE_HPP

#include <iostream>
#include <type_traits>

namespace meevax { inline namespace type_traits
{
  template <typename T, typename = void>
  struct is_stream_insertable
    : public std::false_type
  {};

  template <typename T>
  struct is_stream_insertable<T, std::void_t<decltype(std::declval<std::ostream&>() << std::declval<const T&>())>>
    : public std::true_type
  {};
}} // namespace meevax::concepts

#endif // MEEVAX_TYPE_TRAITS_IS_STREAM_INSERTABLE_HPP
