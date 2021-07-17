#ifndef INCLUDED_MEEVAX_MEMORY_LITERAL_HPP
#define INCLUDED_MEEVAX_MEMORY_LITERAL_HPP

namespace meevax
{
inline namespace memory
{
  constexpr auto operator ""_KiB(unsigned long long size)
  {
    return size * 1024;
  }

  constexpr auto operator ""_MiB(unsigned long long size)
  {
    return size * 1024 * 1024;
  }
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_LITERAL_HPP
