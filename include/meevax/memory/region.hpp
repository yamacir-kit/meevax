#ifndef INCLUDED_MEEVAX_MEMORY_REGION_HPP
#define INCLUDED_MEEVAX_MEMORY_REGION_HPP

#include <cstdint> // std::uintptr_t
#include <functional> // std::less
#include <type_traits>

#include <meevax/memory/deallocator.hpp>
#include <meevax/memory/marker.hpp>

namespace meevax
{
inline namespace memory
{
  class region : public marker
  {
    pointer<void> base, derived = nullptr;

    std::size_t size;

    deallocator<void>::signature deallocate = nullptr;

  public:
    explicit region(pointer<void> const base, std::size_t const size)
      : base { base }
      , size { size }
    {}

    ~region()
    {
      if (assigned())
      {
        deallocate(derived);
      }
    }

    auto lower_bound() const noexcept
    {
      return reinterpret_cast<std::uintptr_t>(base);
    }

    auto upper_bound() const noexcept
    {
      return lower_bound() + size;
    }

    auto controls(std::uintptr_t const k) const noexcept
    {
      return lower_bound() <= k and k < upper_bound();
    }

    auto controls(pointer<void> const derived) const noexcept
    {
      return controls(reinterpret_cast<std::uintptr_t>(derived));
    }

    constexpr bool assigned() const noexcept
    {
      return derived and deallocate;
    }

    void reset(decltype(derived) x, decltype(deallocate) f) noexcept
    {
      derived = x;
      deallocate = f;
    }

    void release()
    {
      assert(marked());

      if (assigned())
      {
        deallocate(derived);
      }

      reset(nullptr, nullptr);

      size = 0;
    }
  };
} // namespace memory
} // namespace meevax

namespace std
{
  template <>
  struct less<meevax::pointer<meevax::region>>
  {
    bool operator ()(meevax::const_pointer<meevax::region> x,
                     meevax::const_pointer<meevax::region> y) const
    {
      return (*x).upper_bound() <= (*y).lower_bound();
    }
  };
} // namespace std

#endif // INCLUDED_MEEVAX_MEMORY_REGION_HPP
