#ifndef INCLUDED_MEEVAX_MEMORY_CONTROLLER_HPP
#define INCLUDED_MEEVAX_MEMORY_CONTROLLER_HPP

#include <meevax/memory/marker.hpp>
#include <meevax/memory/void_pointer.hpp>

namespace meevax
{
inline namespace memory
{
  struct controller
  {
    using pointer = typename std::add_pointer<controller>::type;

    using signature = void (*)(void_pointer const);

  public:
    void_pointer base, derived = nullptr;

    std::size_t size;

    marker reachable;

    signature custom_delete = nullptr;

  public:
    explicit controller(void_pointer const base, std::size_t const size)
      : base { base }
      , size { size }
    {}

    ~controller()
    {
      release();
    }

    constexpr auto lower_bound() const noexcept
    {
      return reinterpret_cast<std::uintptr_t>(base);
    }

    constexpr auto upper_bound() const noexcept
    {
      return lower_bound() + size;
    }

    constexpr auto controls(std::uintptr_t const k) const noexcept
    {
      return lower_bound() <= k and k < upper_bound();
    }

    constexpr auto controls(void_pointer const p) const noexcept
    {
      return controls(reinterpret_cast<std::uintptr_t>(p));
    }

    constexpr bool has_custom_deleter() const noexcept
    {
      return custom_delete;
    }

    void release()
    {
      if (derived and custom_delete)
      {
        custom_delete(derived);
      }

      derived = base = nullptr;

      size = 0;
    }
  };
} // namespace memory
} // namespace meevax

namespace std
{
  template <>
  struct less<meevax::controller::pointer>
  {
    constexpr bool operator ()(meevax::controller const* x, meevax::controller const* y) const
    {
      return (*x).upper_bound() <= (*y).lower_bound();
    }
  };
} // namespace std

#endif // INCLUDED_MEEVAX_MEMORY_CONTROLLER_HPP
