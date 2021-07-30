#ifndef INCLUDED_MEEVAX_MEMORY_REGION_HPP
#define INCLUDED_MEEVAX_MEMORY_REGION_HPP

#include <cstdint> // std::uintptr_t
#include <functional> // std::less

#include <meevax/memory/deallocator.hpp>
#include <meevax/memory/marker.hpp>
#include <meevax/utility/debug.hpp>

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
      release();
    }

    auto lower_bound() const noexcept
    {
      return reinterpret_cast<std::uintptr_t>(base);
    }

    auto upper_bound() const noexcept
    {
      return lower_bound() + size;
    }

    auto contains(std::uintptr_t const k) const noexcept
    {
      return lower_bound() <= k and k < upper_bound();
    }

    auto contains(pointer<void> const derived) const noexcept
    {
      return contains(reinterpret_cast<std::uintptr_t>(derived));
    }

    auto assigned() const noexcept
    {
      return derived and deallocate;
    }

    auto reset(pointer<void> const x, deallocator<void>::signature const f) noexcept
    {
      if (not assigned())
      {
        derived = x;
        deallocate = f;
      }

      return this;
    }

    void release()
    {
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
