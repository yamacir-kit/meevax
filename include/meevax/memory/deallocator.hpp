#ifndef INCLUDED_MEEVAX_MEMORY_DEALLOCATOR_HPP
#define INCLUDED_MEEVAX_MEMORY_DEALLOCATOR_HPP

#include <meevax/memory/pointer.hpp>

namespace meevax
{
inline namespace memory
{
  template <typename T>
  struct deallocator
  {
    using signature = void (*)(pointer<void> const);

    static void deallocate(pointer<void> const p)
    {
      delete static_cast<const pointer<const T>>(p);
    }
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_DEALLOCATOR_HPP
