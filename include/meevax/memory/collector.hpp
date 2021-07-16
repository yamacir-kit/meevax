#ifndef INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP
#define INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP

#include <cassert>
#include <cstddef>
#include <limits>
#include <map>
#include <memory>
#include <mutex>
#include <set>

#include <meevax/memory/region.hpp>
#include <meevax/string/header.hpp>
#include <meevax/utility/debug.hpp>

namespace meevax
{
inline namespace memory
{
  /* ---- Acknowledgement ------------------------------------------------------
   *
   *  The class is based on the implementation of gc_ptr written by
   *  William E. Kempf and posted to CodeProject.
   *
   *  - https://www.codeproject.com/Articles/912/A-garbage-collection-framework-for-C
   *  - https://www.codeproject.com/Articles/938/A-garbage-collection-framework-for-C-Part-II
   *
   * ------------------------------------------------------------------------ */
  class collector // A mark-and-sweep garbage collector.
  {
  public:
    struct object
    {
    protected:
      explicit object()
      {
        if (auto const lock = std::unique_lock(resource); lock)
        {
          objects.emplace(this, nullptr);
        }
      }

      ~object()
      {
        if (auto const lock = std::unique_lock(resource); lock)
        {
          objects.erase(this);
        }
      }

      void reset(pointer<void> const derived, deallocator<void>::signature const deallocate)
      {
        if (auto const lock = std::unique_lock(resource); lock)
        {
          objects[this] = collector::reset(derived, deallocate);
        }
      }

      template <typename Pointer>
      void reset(Pointer const derived)
      {
        reset(derived, deallocator<typename std::pointer_traits<Pointer>::element_type>::deallocate);
      }
    };

  private:
    static inline std::mutex resource;

    static inline std::map<pointer<object>, pointer<region>> objects;

    static inline std::set<pointer<region>> regions;

    static inline std::size_t allocation;

    static inline std::size_t threshold;

  public:
    explicit collector();

    explicit collector(collector &&) = delete;

    explicit collector(collector const&) = delete;

    ~collector();

    collector & operator =(collector &&) = delete;

    collector & operator =(collector const&) = delete;

    void clear()
    {
      for (auto iter = std::begin(regions); iter != std::end(regions); )
      {
        assert(*iter);

        if (pointer<region> region = *iter; region->assigned())
        {
          delete region;
          iter = regions.erase(iter);
        }
        else
        {
          ++iter;
        }
      }
    }

    auto collect()
    {
      auto const before = count();

      if (auto const lock = std::unique_lock(resource); lock)
      {
        mark(), sweep();

        allocation = 0;
      }

      return before - count();
    }

    auto count() const noexcept -> std::size_t
    {
      return std::size(regions);
    }

    auto erase(decltype(regions)::iterator iter)
    {
      return regions.erase(iter);
    }

    auto insert(pointer<void> const base, std::size_t const size) -> decltype(auto)
    {
      allocation += size;
      return regions.insert(new region(base, size));
    }

    void mark()
    {
      marker::toggle();

      for (auto [derived, region] : objects)
      {
        if (region and not region->marked() and region_of(derived) == std::end(regions))
        {
          traverse(region);
        }
      }
    }

    auto overflow() const noexcept
    {
      return threshold < allocation;
    }

    static auto region_of(pointer<void> const interior) -> decltype(regions)::iterator
    {
      region dummy { interior, 0 };

      if (auto iter = regions.lower_bound(&dummy); iter != std::end(regions) and (**iter).contains(interior))
      {
        return iter;
      }
      else
      {
        return std::end(regions);
      }
    }

    static auto reset(pointer<void> const derived, deallocator<void>::signature const deallocate) -> pointer<region>
    {
      if (auto const lock = std::unique_lock(resource); lock and derived)
      {
        auto const iter = region_of(derived);

        assert(iter != std::end(regions));
        assert(deallocate);

        return (*iter)->reset(derived, deallocate);
      }
      else
      {
        return nullptr;
      }
    }

    void reset_threshold(std::size_t const size = std::numeric_limits<std::size_t>::max())
    {
      if (auto const lock = std::unique_lock(resource); lock)
      {
        threshold = size;
      }
    }

    void sweep()
    {
      for (auto iter = std::begin(regions); iter != std::end(regions); )
      {
        assert(*iter);

        if (pointer<region> region = *iter; not region->marked())
        {
          if (region->assigned())
          {
            delete region;
            iter = regions.erase(iter);
            continue;
          }
          else
          {
            region->mark();
          }
        }

        ++iter;
      }
    }

    void traverse(pointer<region> const the_region)
    {
      if (the_region and not the_region->marked())
      {
        the_region->mark();

        auto lower = objects.lower_bound(reinterpret_cast<pointer<object>>(the_region->lower_bound()));
        auto upper = objects.lower_bound(reinterpret_cast<pointer<object>>(the_region->upper_bound()));

        for (auto iter = lower; iter != upper; ++iter)
        {
          traverse(iter->second);
        }
      }
    }
  } static gc;

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

meevax::pointer<void> operator new(std::size_t const, meevax::collector &);

void operator delete(meevax::pointer<void> const, meevax::collector &) noexcept;

#endif // INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP
