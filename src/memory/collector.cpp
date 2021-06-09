#include <meevax/memory/collector.hpp>

namespace meevax
{
inline namespace memory
{
  static std::size_t count = 0;

  collector::collector()
  {
    if (not count++)
    {
      collectables = {};

      regions = {};

      collecting = false;

      newly_allocated = 0;

      threshold = std::numeric_limits<std::size_t>::max();

      std::cout << header(__func__) << "ready." << std::endl;
    }
  }

  collector::~collector()
  {
    if (not --count)
    {
      std::cout << header(__func__) << "collecting objects" << std::endl;

      auto const collectables_size = std::size(collectables);
      auto const regions_size      = std::size(regions);

      collect();
      collect(); // ???

      std::cout << header("")       << "  collectables = " << collectables_size << " => " << std::size(collectables) << "\n"
                << header("")       << "  regions = " << regions_size << " => " << std::size(regions) << std::endl;

      // for (auto iter = std::begin(regions); iter != std::end(regions); )
      // {
      //   assert(*iter);
      //
      //   if (region::pointer region = *iter; region->assigned())
      //   {
      //     delete region;
      //     iter = regions.erase(iter);
      //   }
      //   else
      //   {
      //     ++iter;
      //   }
      // }

      assert(std::size(collectables) == 0);
      assert(std::size(regions) == 0);
    }
  }
} // namespace memory
} // namespace meevax

meevax::void_pointer operator new(std::size_t const size, meevax::collector & gc)
{
  auto const lock = gc.lock();

  // std::cout << meevax::header(__func__) << "allocating " << size << " bytes of object (" << (gc.newly_allocated + size) << ", " << gc.size() << ")" << std::endl;

  if (gc.newly_allocated += size; gc.threshold < gc.newly_allocated)
  {
    // std::cout << meevax::header(__func__) << "exceeds threthold => invoke collector." << std::endl;
    gc.collect();
  }

  auto p = ::operator new(size);

  gc.insert(p, size);

  return p;
}

void operator delete(meevax::void_pointer const p, meevax::collector & gc) noexcept
{
  auto const lock = gc.lock();

  try
  {
    if (auto const iter = gc.find(p); *iter)
    {
      gc.erase(iter);
    }
  }
  catch (...)
  {}

  ::operator delete(p);
}
