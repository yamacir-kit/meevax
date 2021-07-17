#ifndef INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP
#define INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP

#include <cassert>
#include <cstddef>
#include <limits>
#include <map>
#include <mutex>
#include <new>
#include <set>

#include <meevax/memory/literal.hpp>
#include <meevax/memory/region.hpp>
#include <meevax/string/header.hpp>
#include <meevax/utility/debug.hpp>

namespace meevax
{
inline namespace memory
{
  /* ---- Acknowledgement ------------------------------------------------------
   *
   *  This mark-and-sweep garbage collector is based on the implementation of
   *  gc_ptr written by William E. Kempf and posted to CodeProject.
   *
   *  - https://www.codeproject.com/Articles/912/A-garbage-collection-framework-for-C
   *  - https://www.codeproject.com/Articles/938/A-garbage-collection-framework-for-C-Part-II
   *
   * ------------------------------------------------------------------------ */
  class collector
  {
  public:
    using is_always_equal = std::true_type;

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

      explicit object(pointer<void> const derived, deallocator<void>::signature const deallocate)
      {
        if (auto const lock = std::unique_lock(resource); lock)
        {
          objects.emplace(this, collector::reset(derived, deallocate));
        }
      }

      template <typename Pointer>
      explicit object(Pointer const derived)
        : object { derived, deallocator<typename std::pointer_traits<Pointer>::element_type>::deallocate }
      {}

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

    auto allocate(std::size_t const size)
    {
      if (auto data = ::operator new(size); data)
      {
        if (overflow())
        {
          collect();
        }

        allocation += size;

        regions.insert(new region(data, size));

        return data;
      }
      else
      {
        throw std::bad_alloc();
      }
    }

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

    auto collect() -> std::size_t
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

    auto deallocate(pointer<void> const data, std::size_t const = 0)
    {
      try
      {
        if (auto const iter = region_of(data); *iter)
        {
          regions.erase(iter);
        }
      }
      catch (...)
      {}

      ::operator delete(data);
    }

    auto mark() -> void
    {
      marker::toggle();

      for (auto [derived, region] : objects)
      {
        if (region and not region->marked() and region_of(derived) == std::cend(regions))
        {
          traverse(region);
        }
      }
    }

    auto overflow() const noexcept -> bool
    {
      return threshold < allocation;
    }

    static auto region_of(pointer<void> const interior) -> decltype(regions)::iterator
    {
      region dummy { interior, 0 };

      if (auto iter = regions.lower_bound(&dummy); iter != std::cend(regions) and (**iter).contains(interior))
      {
        return iter;
      }
      else
      {
        return std::cend(regions);
      }
    }

    static auto reset(pointer<void> const derived, deallocator<void>::signature const deallocate) -> pointer<region>
    {
      if (auto const lock = std::unique_lock(resource); lock and derived)
      {
        auto const iter = region_of(derived);

        assert(iter != std::cend(regions));
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
} // namespace memory
} // namespace meevax

auto operator new(std::size_t const, meevax::collector &) -> meevax::pointer<void>;

void operator delete(meevax::pointer<void> const, meevax::collector &) noexcept;

#endif // INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP
