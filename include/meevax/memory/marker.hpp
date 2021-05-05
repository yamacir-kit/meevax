#ifndef INCLUDED_MEEVAX_MEMORY_MARKER_HPP
#define INCLUDED_MEEVAX_MEMORY_MARKER_HPP

namespace meevax
{
inline namespace memory
{
  class marker
  {
    static inline auto phase = false;

    bool value;

  public:
    explicit marker(bool value = phase)
      : value { value }
    {}

    auto mark() noexcept
    {
      return value = phase;
    }

    auto unmark() noexcept
    {
      return value = not phase;
    }

    auto marked() const noexcept
    {
      return value == phase;
    }

    static auto flip() noexcept
    {
      return phase = not phase;
    }

    explicit operator bool() const noexcept
    {
      return marked();
    }
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_MARKER_HPP
