#ifndef INCLUDED_MEEVAX_TYPE_TRAITS_VOID_T_HPP
#define INCLUDED_MEEVAX_TYPE_TRAITS_VOID_T_HPP

#include <type_traits>

namespace meevax { inline namespace type_traits
{
  #if __cpp_lib_void_t

  template <typename... Ts>
  using void_t = std::void_t<Ts...>;

  #else // __cpp_lib_void_t

  template <typename...>
  using void_t = void;

  #endif // __cpp_lib_void_t
}} // namespace meevax::type_traits

#endif // INCLUDED_MEEVAX_TYPE_TRAITS_VOID_T_HPP
