#include <meevax/memory/collector.hpp>

namespace meevax
{
inline namespace memory
{
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
