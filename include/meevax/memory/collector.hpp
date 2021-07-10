#ifndef INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP
#define INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP

#include <cassert>
#include <cstddef>
#include <iostream>
#include <limits>
#include <map>
#include <memory>
#include <mutex>
#include <set>

#include <meevax/memory/region.hpp>
#include <meevax/string/header.hpp>

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
        roots.emplace(this, nullptr);
      }

      ~root()
      {
        auto const locking = lock();
        roots.erase(this);
      }

      void reset(pointer<void> const derived, deallocator<void>::signature const deallocate)
      {
        auto const locking = lock();
        roots[this] = collector::reset(derived, deallocate);
      }

      template <typename Pointer>
      void reset(Pointer const derived)
      {
        reset(derived, deallocator<typename std::pointer_traits<Pointer>::element_type>::deallocate);
      }
    };

  private:
    static inline std::mutex resource;

    static inline std::map<pointer<root>, pointer<region>> roots;

    static inline std::set<pointer<region>> regions;

    static inline bool collecting;

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
      auto const size = regions.size();

      if (auto const locking = lock(); not collecting)
      {
        collecting = true;

        mark(), sweep();

        collecting = false;

        newly_allocated = 0;
      }

      return size - regions.size();
    }

    auto erase(decltype(regions)::iterator iter) -> decltype(auto)
    {
      return regions.erase(iter);
    }

    static auto find(pointer<void> const interior)
    {
      const auto dummy = std::make_unique<region>(interior, 0);

      if (auto iter = regions.lower_bound(dummy.get()); iter != std::end(regions) and (**iter).controls(interior))
      {
        return iter;
      }
      else
      {
        return std::end(regions);
      }
    }

    auto insert(pointer<void> const base, std::size_t const size) -> decltype(auto)
    {
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

      for (auto [x, region] : roots)
      {
        if (region and not region->marked() and find(x) == std::end(regions))
        {
          traverse(region);
        }
      }
    }

    auto overflow(std::size_t const size = 0)
    {
      return threshold < newly_allocated + size;
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

        auto iter = find(derived);

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

    auto size()
    {
      return regions.size();
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

        auto lower = roots.lower_bound(reinterpret_cast<pointer<root>>(the_region->lower_bound()));
        auto upper = roots.lower_bound(reinterpret_cast<pointer<root>>(the_region->upper_bound()));

        for (auto iter = lower; iter != upper; ++iter)
        {
          traverse(iter->second);
        }
      }
    }
  } static gc;

  constexpr auto operator ""_MiB(unsigned long long n)
  {
    return n * 1024 * 1024;
  }
} // namespace memory
} // namespace meevax

meevax::pointer<void> operator new(std::size_t const, meevax::collector &);

void operator delete(meevax::pointer<void> const, meevax::collector &) noexcept;

#endif // INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP
