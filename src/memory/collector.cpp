
#include <meevax/memory/collector.hpp>

namespace meevax
{
inline namespace memory
{
  static std::size_t reference_count = 0;

  collector::collector()
  {
    if (not reference_count++)
    {
      objects = {};

      regions = {};

      allocation = 0;

      // threshold = std::numeric_limits<std::size_t>::max();
      threshold = 8_MiB;
    }
  }

  collector::~collector()
  {
    if (not --reference_count)
    {
      /* ---- NOTE -------------------------------------------------------------
       *
       *  We're using collect instead of clear to check that all objects can be
       *  collected. If speed is a priority, clear should be used here.
       *
       * -------------------------------------------------------------------- */

      collect();
      collect(); // XXX: vector elements

      assert(std::size(objects) == 0);
      assert(std::size(regions) == 0);
    }
  }
} // namespace memory
} // namespace meevax

auto operator new(std::size_t const size, meevax::collector & gc) -> meevax::pointer<void>
{
  return gc.allocate(size);
}

void operator delete(meevax::pointer<void> const data, meevax::collector & gc) noexcept
{
  gc.deallocate(data);
}
