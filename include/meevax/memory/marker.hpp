#ifndef INCLUDED_MEEVAX_MEMORY_MARKER_HPP
#define INCLUDED_MEEVAX_MEMORY_MARKER_HPP

namespace meevax
{
inline namespace memory
{
  class marker
  {
    static inline bool phase;

    bool value;

  public:
    struct initializer
    {
      explicit initializer();
    };

    explicit marker() noexcept
      : value { phase }
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

    static auto toggle() noexcept
    {
      return phase = not phase;
    }

    explicit operator bool() const noexcept
    {
      return marked();
    }
  };

  static marker::initializer initializer;
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_MARKER_HPP
