#ifndef MEEVAX_CONCEPTS_IS_STREAM_INSERTABLE_HPP
#define MEEVAX_CONCEPTS_IS_STREAM_INSERTABLE_HPP

#include <iostream>
#include <type_traits>

// TODO Rename to OutputStreamable (from Boost.IOStreams)

namespace meevax::concepts
{
  template <typename T, typename = void>
  struct is_stream_insertable
    : public std::false_type
  {};

  template <typename T>
  struct is_stream_insertable<
           T,
           std::void_t<decltype(
             std::declval<std::ostream&>() << std::declval<const T&>()
           )>
         >
    : public std::true_type
  {};
} // namespace meevax::concepts

#endif // MEEVAX_CONCEPTS_IS_STREAM_INSERTABLE_HPP

