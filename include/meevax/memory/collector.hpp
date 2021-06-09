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
    static inline std::mutex resource;

  public:
    static auto lock()
    {
      return std::unique_lock(resource);
    }

    struct collectable
    {
      using pointer = typename std::add_pointer<collectable>::type;

    protected:
      explicit collectable()
      {
        auto const locking = lock();
        collectables.emplace(this, nullptr);
      }

      ~collectable()
      {
        auto const locking = lock();
        collectables.erase(this);
      }

      void reset(void_pointer const derived, deallocator<void>::signature const deallocate)
      {
        auto const locking = lock();
        collectables[this] = collector::reset(derived, deallocate);
      }

      template <typename P>
      void reset(P const derived)
      {
        using element_type = typename std::pointer_traits<P>::element_type;
        reset(derived, deallocator<element_type>::deallocate);
      }
    };

  public: /* ---- DATA MEMBERS ---------------------------------------------- */

    static inline std::map<collectable::pointer, region::pointer> collectables;

    static inline std::set<region::pointer> regions;

    static inline bool collecting;

    static inline std::size_t newly_allocated;

    static inline std::size_t threshold;
    // static inline std::size_t threshold = 1024 * 1024; // = 1 MiB

  public: /* ---- CONSTRUCTORS AND DESTRUCTORS ------------------------------ */

    explicit collector();

    explicit collector(collector &&) = delete;
    explicit collector(collector const&) = delete;

    collector & operator =(collector &&) = delete;
    collector & operator =(collector const&) = delete;

    ~collector();

  public:
    static auto find(void_pointer const x)
    {
      const auto dummy = std::make_unique<region>(x, 0);

      if (auto iter = regions.lower_bound(dummy.get()); iter != std::end(regions) and (**iter).controls(x))
      {
        return iter;
      }
      else
      {
        return std::end(regions);
      }
    }

    template <typename... Ts>
    auto is_root(Ts&&... xs) const
    {
      return find(std::forward<decltype(xs)>(xs)...) == std::end(regions);
    }

    static region::pointer reset(void_pointer const derived, deallocator<void>::signature const deallocate)
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

    // NOTE: v1
    // void traverse(region::pointer const root)
    // {
    //   // std::size_t size = 0;
    //
    //   if (root and not root->marked())
    //   {
    //     root->mark();
    //
    //     for (auto && [x, region] : roots)
    //     {
    //       // std::cout << "\r\x1b[K; traverse\t\t; [" << ++size << "/" << std::size(roots) << "] (" << count() << " marked)" << std::flush;
    //
    //       if (root->controls(x))
    //       {
    //         traverse(region);
    //       }
    //     }
    //   }
    // }

    // NOTE: v2
    // void traverse(region::pointer const node)
    // {
    //   if (node and not node->marked())
    //   {
    //     node->mark();
    //
    //     auto iter = roots.lower_bound(static_cast<root::pointer>(node->base));
    //
    //     if (iter != std::end(roots))
    //     {
    //       if (auto && [x, region] = *iter; node->controls(x))
    //       {
    //         traverse(region);
    //       }
    //
    //       ++iter;
    //     }
    //
    //     for (; iter != std::end(roots) and node->controls(iter->first); ++iter)
    //     {
    //       traverse(iter->second);
    //     }
    //   }
    // }

    // NOTE: v3
    void traverse(region::pointer const the_region)
    {
      if (the_region and not the_region->marked())
      {
        the_region->mark();

        auto lower = collectables.lower_bound(reinterpret_cast<collectable::pointer>(the_region->lower_bound()));
        auto upper = collectables.lower_bound(reinterpret_cast<collectable::pointer>(the_region->upper_bound()));

        for (auto iter = lower; iter != upper; ++iter)
        {
          traverse(iter->second);
        }
      }
    }

    auto mark()
    {
      marker::toggle();

      // std::size_t size = 0;

      for (auto [x, region] : collectables)
      {
        // std::cout << "\r\x1b[K; mark\t\t\t; [" << ++size << "/" << std::size(collectables) << "] (" << count() << " marked)" << std::flush;

        if (region and not region->marked() and is_root(x)) // = is_unmarked_root
        {
          traverse(region);
        }
      }

      // std::cout << std::endl;
    }

    auto sweep()
    {
      // std::size_t size = std::size(regions);

      // std::size_t deleted = 0;
      // std::size_t skipped = 0;
      // std::size_t remains = 0;

      for (auto iter = std::begin(regions); iter != std::end(regions);
           // std::cout << "\r\x1b[K; sweep\t\t\t; ["
           //           << std::distance(std::begin(regions), iter)
           //           << "/"
           //           << std::size(regions)
           //           << "] ("
           //           << size
           //           << " => "
           //           << std::size(regions)
           //           << ")"
           //           // << ", " << "deleted = " << deleted
           //           // << ", " << "skipped = " << skipped
           //           // << ", " << "remains = " << remains
           //           << std::flush
                     )
      {
        assert(*iter);

        if (region::pointer c = *iter; not c->marked())
        {
          if (c->assigned())
          {
            delete c;
            iter = regions.erase(iter);
            // ++deleted;
            continue;
          }
          else
          {
            // ++skipped;
            c->mark();
          }
        }

        // ++remains;
        ++iter;
      }

      // std::cout << std::endl;
    }

    void collect()
    {
      if (auto const locking = lock(); not collecting)
      {
        collecting = true;

        mark(), sweep();

        collecting = false;

        newly_allocated = 0;
      }
    }

    void reset_threshold(std::size_t const size = std::numeric_limits<std::size_t>::max())
    {
      auto const locking = lock();
      threshold = size;
    }

    template <typename... Ts>
    decltype(auto) insert(Ts&&... xs)
    {
      return regions.insert(new region(std::forward<decltype(xs)>(xs)...));
    }

    template <typename... Ts>
    decltype(auto) erase(Ts&&... xs)
    {
      return regions.erase(std::forward<decltype(xs)>(xs)...);
    }

  public: /* ---- DEBUG TOOLS ----------------------------------------------- */

    static auto size()
    {
      return regions.size();
    }

    auto count_unmarked_collectables()
    {
      return std::count_if(std::begin(collectables), std::end(collectables), [](auto const& each)
             {
               auto const* const c = std::get<1>(each);
               return c and c->marked();
             });
    }

    auto count_unmarked_regions() const -> std::size_t
    {
      return std::count_if(std::begin(regions), std::end(regions), [](auto const& each)
             {
               return each and each->marked();
             });
    }

    auto print() const
    {
      std::cout << "; GC-DETAIL" << std::endl;
      std::cout << ";   collectables" << std::endl;
      std::cout << ";     size: " << std::size(collectables) << std::endl;
      std::cout << ";   regions" << std::endl;
      std::cout << ";     size: " << std::size(regions) << std::endl;
    }

  } static gc;
} // namespace memory
} // namespace meevax

meevax::void_pointer operator new(std::size_t const, meevax::collector &);

void operator delete(meevax::void_pointer const, meevax::collector &) noexcept;

#endif // INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP
