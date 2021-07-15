#ifndef INCLUDED_MEEVAX_MEMORY_POINTER_HPP
#define INCLUDED_MEEVAX_MEMORY_POINTER_HPP

#include <type_traits>

namespace meevax
{
inline namespace memory
{
  template <typename... Ts>
  using pointer = typename std::add_pointer<Ts...>::type;

  template <typename... Ts>
  using const_pointer = typename std::add_const<pointer<Ts...>>::type;

  static_assert(std::is_same<void      *      ,       pointer<      void>>::value);
  static_assert(std::is_same<void      * const, const pointer<      void>>::value);
  static_assert(std::is_same<void      * const, const_pointer<      void>>::value);
  static_assert(std::is_same<void const*      ,       pointer<const void>>::value);
  static_assert(std::is_same<void const* const, const pointer<const void>>::value);
  static_assert(std::is_same<void const* const, const_pointer<const void>>::value);
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_POINTER_HPP
