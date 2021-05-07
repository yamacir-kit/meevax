#ifndef INCLUDED_MEEVAX_MEMORY_DEALLOCATOR_HPP
#define INCLUDED_MEEVAX_MEMORY_DEALLOCATOR_HPP

#include <meevax/memory/void_pointer.hpp>

namespace meevax
{
inline namespace memory
{
  template <typename T>
  struct deallocator
  {
    using signature = void (*)(void_pointer const);

    static void deallocate(void_pointer const p)
    {
      delete static_cast<T const*>(p);
    }
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_DEALLOCATOR_HPP
