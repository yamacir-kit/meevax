#ifndef INCLUDED_MEEVAX_MEMORY_VOID_POINTER_HPP
#define INCLUDED_MEEVAX_MEMORY_VOID_POINTER_HPP

#include <type_traits>

namespace meevax
{
inline namespace memory
{
  using void_pointer = typename std::add_pointer<void>::type;
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_VOID_POINTER_HPP
