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

#define LINE() std::cout << "; " __FILE__ ":" << __LINE__ << std::endl

namespace meevax
{
inline namespace memory
{
  class collector // A mark-and-sweep garbage collector.
  {
  public:
    struct root
    {
      using pointer = typename std::add_pointer<root>::type;

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

      void reset(void_pointer const derived, deallocator<void>::signature const deallocate)
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

    static inline std::map<root::pointer, region::pointer> roots;

    static inline std::set<region::pointer> regions;

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

        if (region::pointer region = *iter; region->assigned())
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

      std::cout << header(__func__) << (size - regions.size()) << " objects collected." << std::endl;

      return size - regions.size();
    }

    auto erase(decltype(regions)::iterator iter) -> decltype(auto)
    {
      return regions.erase(iter);
    }

    static auto find(void_pointer const interior)
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

    auto insert(void_pointer const base, std::size_t const size) -> decltype(auto)
    {
      newly_allocated += size;
      return regions.insert(new region(base, size));
    }

    auto is_root(void_pointer const interior) const
    {
      return find(interior) == std::end(regions);
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
        if (region and not region->marked() and is_root(x)) // = is_unmarked_root
        {
          traverse(region);
        }
      }
    }

    auto overflow(std::size_t const size)
    {
      return threshold < newly_allocated + size;
    }

    static auto reset(void_pointer const derived, deallocator<void>::signature const deallocate) -> region::pointer
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

        region::pointer the_region = *iter;

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

        if (region::pointer c = *iter; not c->marked())
        {
          if (c->assigned())
          {
            delete c;
            iter = regions.erase(iter);
            continue;
          }
          else
          {
            c->mark();
          }
        }

        ++iter;
      }
    }

    void traverse(region::pointer const the_region)
    {
      if (the_region and not the_region->marked())
      {
        the_region->mark();

        auto lower = roots.lower_bound(reinterpret_cast<root::pointer>(the_region->lower_bound()));
        auto upper = roots.lower_bound(reinterpret_cast<root::pointer>(the_region->upper_bound()));

        for (auto iter = lower; iter != upper; ++iter)
        {
          traverse(iter->second);
        }
      }
    }
  } static gc;
} // namespace memory
} // namespace meevax

meevax::void_pointer operator new(std::size_t const, meevax::collector &);

void operator delete(meevax::void_pointer const, meevax::collector &) noexcept;

#endif // INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP
