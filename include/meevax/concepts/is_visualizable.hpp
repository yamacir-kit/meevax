#ifndef MEEVAX_CONCEPTS_IS_VISUALIZABLE_HPP
#define MEEVAX_CONCEPTS_IS_VISUALIZABLE_HPP

#include <type_traits>

#include <meevax/visual/surface.hpp>

namespace meevax::concepts
{
  template <typename T, typename = void>
  struct is_visualizable
    : public std::false_type
  {};

  template <typename T>
  struct is_visualizable<
           T,
           std::void_t<decltype(
             visualize(std::declval<visual::surface&>(), std::declval<T&>())
           )>
         >
    : public std::true_type
  {};
} // namespace meevax::concepts

#endif // MEEVAX_CONCEPTS_IS_VISUALIZABLE_HPP

