#include <cstdint>

#include <meevax/memory/marker.hpp>

namespace meevax
{
inline namespace memory
{
  std::size_t count = 0;

  marker::initializer::initializer()
  {
    if (not count++)
    {
      phase = {};
    }
  }
} // namespace memory
} // namespace meevax
