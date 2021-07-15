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
  class collector // A mark-and-sweep garbage collector.
  {
  public:
    struct root
    {
    protected:
      explicit root()
      {
        auto const locking = lock();
        objects.emplace(this, nullptr);
      }

      ~root()
      {
        auto const locking = lock();
        objects.erase(this);
      }

      void reset(pointer<void> const derived, deallocator<void>::signature const deallocate)
      {
        auto const locking = lock();
        objects[this] = collector::reset(derived, deallocate);
      }

      template <typename Pointer>
      void reset(Pointer const derived)
      {
        reset(derived, deallocator<typename std::pointer_traits<Pointer>::element_type>::deallocate);
      }
    };

  private:
    static inline std::mutex resource;

    static inline std::map<pointer<root>, pointer<region>> objects;

    static inline std::set<pointer<region>> regions;

    static inline bool collecting;

    static inline std::size_t whole_size;

    static inline std::size_t newly_allocated;

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

      if (auto const locking = lock(); not collecting)
      {
        collecting = true;

        mark(), sweep();

        collecting = false;

        newly_allocated = 0;
      }

      return before - count();
    }

    auto count() const noexcept -> std::size_t
    {
      return regions.size();
    }

    auto erase(decltype(regions)::iterator iter)
    {
      whole_size -= (*iter)->size;
      return regions.erase(iter);
    }

    auto insert(pointer<void> const base, std::size_t const size) -> decltype(auto)
    {
      whole_size += size;
      newly_allocated += size;
      return regions.insert(new region(base, size));
    }

    static auto lock() -> std::unique_lock<std::mutex>
    {
      return std::unique_lock(resource);
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
      return threshold < newly_allocated;
    }

    static auto region_of(pointer<void> const interior) -> decltype(regions)::iterator
    {
      const auto dummy = std::make_unique<region>(interior, 0);

      if (auto iter = regions.lower_bound(dummy.get()); iter != std::end(regions) and (**iter).contains(interior))
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
      if (not derived)
      {
        return nullptr;
      }
      else
      {
        auto const locking = lock();

        auto iter = region_of(derived);

        assert(iter != std::end(regions));
        assert(deallocate);

        pointer<region> the_region = *iter;

        if (deallocate and not the_region->assigned())
        {
          the_region->reset(derived, deallocate);
        }

        return the_region;
      }
    }

    void reset_threshold(std::size_t const size = std::numeric_limits<std::size_t>::max())
    {
      auto const locking = lock();
      threshold = size;
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

        auto lower = objects.lower_bound(reinterpret_cast<pointer<root>>(the_region->lower_bound()));
        auto upper = objects.lower_bound(reinterpret_cast<pointer<root>>(the_region->upper_bound()));

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
