/*
   Copyright 2018-2025 Tatsuya Yamasaki.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

#ifndef INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP
#define INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP

#include <dlfcn.h> // dlopen, dlclose, dlerror

#include <memory> // std::allocator
#include <stack>
#include <unordered_map>

#include <meevax/iostream/escape_sequence.hpp>
#include <meevax/iostream/lexical_cast.hpp>
#include <meevax/memory/literal.hpp>
#include <meevax/memory/nan_boxing_pointer.hpp>
#include <meevax/memory/pointer_set.hpp>
#include <meevax/type_traits/is_equality_comparable.hpp>
#include <meevax/type_traits/is_output_streamable.hpp>
#include <meevax/utility/demangle.hpp>

namespace meevax::inline memory
{
  struct direct_initialization_tag
  {
    explicit direct_initialization_tag() = default;
  };

  inline constexpr direct_initialization_tag direct_initialization {};

  struct list_initialization_tag
  {
    explicit list_initialization_tag() = default;
  };

  inline constexpr list_initialization_tag list_initialization {};

  template <typename T>
  struct equivalence
  {
    static inline constexpr auto strictness = 0;
  };

  /*
     This mark-and-sweep garbage collector is based on the implementation of
     gc_ptr written by William E. Kempf and posted to CodeProject.

     - https://www.codeproject.com/Articles/912/A-garbage-collection-framework-for-C
     - https://www.codeproject.com/Articles/938/A-garbage-collection-framework-for-C-Part-II
  */
  template <typename Top, typename... Ts>
  struct collector
  {
    struct top
    {
      virtual ~top() = default;

      virtual auto equal1(Top const* x) const -> bool = 0;

      virtual auto equal2(Top const* x) const -> bool = 0;

      virtual auto extent() const noexcept -> std::pair<void const*, std::size_t> = 0;

      virtual auto type() const noexcept -> std::type_info const& = 0;

      virtual auto write(std::ostream & o) const -> std::ostream & = 0;
    };

    static inline auto cleared = false;

    template <typename Allocator>
    struct stateful : public Allocator
    {
      ~stateful()
      {
        /*
           Execute clear before any static allocator is destroyed. Otherwise,
           when the destructor of the collector executes clear, the collector
           may touch the freed memory of the stateful allocator.
        */
        if (not std::exchange(cleared, true))
        {
          clear();
        }
      }
    };

    template <typename Bound, typename AllocatorTraits>
    struct binder : public virtual Top
                  , public Bound
    {
      using allocator = stateful<typename AllocatorTraits::template rebind_alloc<binder<Bound, AllocatorTraits>>>;

      static inline auto a = allocator();

      template <typename... Us>
      explicit constexpr binder(direct_initialization_tag, Us&&... xs)
        : std::conditional_t<std::is_base_of_v<Top, Bound> and std::is_constructible_v<Top, Us...>, Top, Bound>(std::forward<decltype(xs)>(xs)...)
      {}

      template <typename... Us>
      explicit constexpr binder(list_initialization_tag, Us&&... xs)
        : std::conditional_t<std::is_base_of_v<Top, Bound> and std::is_constructible_v<Top, Us...>, Top, Bound> { std::forward<decltype(xs)>(xs)... }
      {}

      template <typename... Us>
      explicit constexpr binder(Us&&... xs)
        : binder { list_initialization, std::forward<decltype(xs)>(xs)... }
      {}

      ~binder() override = default;

      auto equal1([[maybe_unused]] Top const* other) const -> bool override
      {
        if constexpr (is_equality_comparable_v<Bound const&> and equivalence<Bound>::strictness <= 1)
        {
          if (auto const* bound = dynamic_cast<Bound const*>(other); bound)
          {
            return *bound == static_cast<Bound const&>(*this);
          }
          else
          {
            return std::is_same_v<Bound, std::nullptr_t>;
          }
        }
        else
        {
          return false;
        }
      }

      auto equal2([[maybe_unused]] Top const* other) const -> bool override
      {
        if constexpr (is_equality_comparable_v<Bound const&> and equivalence<Bound>::strictness <= 2)
        {
          if (auto const* bound = dynamic_cast<Bound const*>(other); bound)
          {
            return *bound == static_cast<Bound const&>(*this);
          }
          else
          {
            return std::is_same_v<Bound, std::nullptr_t>;
          }
        }
        else
        {
          return false;
        }
      }

      auto extent() const noexcept -> std::pair<void const*, std::size_t> override
      {
        return { static_cast<Bound const*>(this), sizeof(Bound) };
      }

      auto type() const noexcept -> std::type_info const& override
      {
        return typeid(Bound);
      }

      auto write(std::ostream & os) const -> std::ostream & override
      {
        if constexpr (is_output_streamable_v<Bound const&>)
        {
          return os << static_cast<Bound const&>(*this);
        }
        else
        {
          return os << magenta("#,(") << green(typeid(Bound).name()) << faint(" #;", static_cast<Bound const*>(this)) << magenta(")");
        }
      }

      auto operator new(std::size_t) -> void *
      {
        return a.allocate(1);
      }

      auto operator delete(void * data) noexcept -> void
      {
        a.deallocate(reinterpret_cast<typename std::allocator_traits<allocator>::pointer>(data), 1);
      }
    };

    template <typename AllocatorTraits>
    struct binder<Top, AllocatorTraits> : public Top
    {
      using allocator = stateful<typename AllocatorTraits::template rebind_alloc<binder<Top, AllocatorTraits>>>;

      static inline auto a = allocator();

      using Top::Top;

      ~binder() override = default;

      auto operator new(std::size_t) -> void *
      {
        return a.allocate(1);
      }

      auto operator delete(void * data) noexcept -> void
      {
        a.deallocate(reinterpret_cast<typename std::allocator_traits<allocator>::pointer>(data), 1);
      }
    };

    struct mutator : public nan_boxing_pointer<Top, Ts...>
    {
      using pointer = nan_boxing_pointer<Top, Ts...>;

      mutator(std::nullptr_t = nullptr) noexcept
      {}

      mutator(mutator const& other)
        : pointer { other }
      {
        if (*this)
        {
          assert(not mutators.contains(this));
          mutators.insert(this);
        }
      }

      mutator(Top * top)
        : pointer { top }
      {
        if (top)
        {
          assert(not mutators.contains(this));
          mutators.insert(this);
        }
      }

      template <typename T, typename = std::enable_if_t<(std::is_same_v<T, Ts> or ... or std::is_same_v<T, double>)>>
      mutator(T const& datum)
        : pointer { datum }
      {
        assert(pointer::get() == nullptr);
      }

      ~mutator()
      {
        if (pointer::operator bool() and not cleared)
        {
          assert(mutators.contains(this));
          mutators.erase(this);
        }
      }

      auto operator =(mutator const& other) -> auto &
      {
        reset(other);
        return *this;
      }

      auto operator =(std::nullptr_t) -> auto &
      {
        reset();
        return *this;
      }

      auto reset(mutator const& after) -> void
      {
        auto const before = pointer::operator bool();

        pointer::reset(after);

        if (before)
        {
          if (not after)
          {
            assert(mutators.contains(this));
            mutators.erase(this);
          }
        }
        else if (after)
        {
          assert(not mutators.contains(this));
          mutators.insert(this);
        }
      }

      auto reset(std::nullptr_t = nullptr) -> void
      {
        auto const before = pointer::operator bool();

        pointer::reset();

        if (before)
        {
          assert(mutators.contains(this));
          mutators.erase(this);
        }
      }

      template <typename U>
      inline auto as() const -> decltype(auto)
      {
        if constexpr (std::is_same_v<std::decay_t<U>, Top>)
        {
          return pointer::operator *();
        }
        else if constexpr (std::is_class_v<std::decay_t<U>>)
        {
          if (auto data = dynamic_cast<std::add_pointer_t<U>>(pointer::get()); data)
          {
            return *data;
          }
          else
          {
            throw std::runtime_error(lexical_cast<std::string>("no viable conversion from ", demangle(type()), " to ", demangle(typeid(U))));
          }
        }
        else
        {
          return pointer::template as<U>();
        }
      }

      template <typename U>
      inline auto as() -> decltype(auto)
      {
        if constexpr (std::is_same_v<std::decay_t<U>, Top>)
        {
          return pointer::operator *();
        }
        else if constexpr (std::is_class_v<std::decay_t<U>>)
        {
          if (auto data = dynamic_cast<std::add_pointer_t<U>>(pointer::get()); data)
          {
            return *data;
          }
          else
          {
            throw std::runtime_error(lexical_cast<std::string>("no viable conversion from ", demangle(type()), " to ", demangle(typeid(U))));
          }
        }
        else
        {
          return pointer::template as<U>();
        }
      }

      template <typename U>
      inline auto as_const() const -> decltype(auto)
      {
        return as<std::add_const_t<U>>();
      }

      inline auto equal1(mutator const& rhs) const -> bool
      {
        if (pointer::dereferenceable())
        {
          return *this ? pointer::unsafe_get()->equal1(rhs.get()) : rhs.is<std::nullptr_t>();
        }
        else
        {
          return pointer::compare(rhs);
        }
      }

      inline auto equal2(mutator const& rhs) const -> bool
      {
        if (pointer::dereferenceable())
        {
          return *this ? pointer::unsafe_get()->equal2(rhs.get()) : rhs.is<std::nullptr_t>();
        }
        else
        {
          return pointer::compare(rhs);
        }
      }

      template <typename U>
      inline auto is() const
      {
        return type() == typeid(std::decay_t<U>);
      }

      template <typename U,
                typename = std::enable_if_t<std::is_class_v<U>>>
      inline auto is_also() const
      {
        return dynamic_cast<std::add_pointer_t<U>>(pointer::get()) != nullptr;
      }

      inline auto type() const -> std::type_info const&
      {
        if (pointer::dereferenceable())
        {
          return *this ? pointer::unsafe_get()->type() : typeid(std::nullptr_t);
        }
        else
        {
          return pointer::type();
        }
      }

      inline auto write(std::ostream & os) const -> std::ostream &
      {
        if (pointer::dereferenceable())
        {
          return *this ? pointer::unsafe_get()->write(os) : os << magenta("()");
        }
        else
        {
          return pointer::write(os);
        }
      }

      friend auto operator <<(std::ostream & os, mutator const& datum) -> std::ostream &
      {
        return datum.write(os);
      }

      inline auto  begin()       { return *this ? pointer::unsafe_get()-> begin() : typename Top::      iterator(); }
      inline auto  begin() const { return *this ? pointer::unsafe_get()->cbegin() : typename Top::const_iterator(); }
      inline auto cbegin() const { return *this ? pointer::unsafe_get()->cbegin() : typename Top::const_iterator(); }
      inline auto    end()       { return *this ? pointer::unsafe_get()->   end() : typename Top::      iterator(); }
      inline auto    end() const { return *this ? pointer::unsafe_get()->  cend() : typename Top::const_iterator(); }
      inline auto   cend() const { return *this ? pointer::unsafe_get()->  cend() : typename Top::const_iterator(); }
    };

    /*
       https://www.kernel.org/doc/html/latest/arch/x86/x86_64/mm.html

       0x0000'0000'0000'0000 ~ 0x0000'7FFF'FFFF'FFFF
    */
    template <typename T>
    using pointer_set = memory::pointer_set<T const*, std::bit_width(0x7FFFu),
                                                      std::bit_width(0xFFFFu),
                                                      std::bit_width(0xFFFFu)>;

    static inline pointer_set<top> objects {};

    static inline pointer_set<mutator> mutators {};

    static inline std::size_t size = 0;

    static inline std::size_t threshold = 16_MiB;

    static inline std::unordered_map<std::string, std::unique_ptr<void, void (*)(void * const)>> dynamic_linked_libraries {};

    collector() = delete;

    collector(collector &&) = delete;

    collector(collector const&) = delete;

    ~collector() = delete;

    auto operator =(collector &&) -> collector & = delete;

    auto operator =(collector const&) -> collector & = delete;

    template <typename T, typename Allocator = std::allocator<void>, typename... Us>
    static auto make(Us&&... xs) -> mutator
    {
      if constexpr (std::is_class_v<T>)
      {
        if (size += sizeof(T); threshold < size)
        {
          collect();
        }

        if (auto data = new binder<T, std::allocator_traits<Allocator>>(std::forward<decltype(xs)>(xs)...); data)
        {
          objects.insert(data);

          return data;
        }
        else [[unlikely]]
        {
          throw std::bad_alloc();
        }
      }
      else
      {
        return { std::forward<decltype(xs)>(xs)... };
      }
    }

    static auto clear() -> void
    {
      for (auto const& object : objects)
      {
        delete object;
        assert(objects.contains(object));
        objects.erase(object);
      }
    }

    static auto collect() -> void
    {
      size = 0;

      auto live_objects = pointer_set<top>();

      auto static stack = std::vector<top const*>();

      auto mark = [&](auto const& m)
      {
        assert(m);
        assert(m->unsafe_get());

        if (auto p = m->unsafe_get(); live_objects.insert(p))
        {
          stack.push_back(p);
        }
      };

      for (auto const& m : mutators)
      {
        if (is_root(m))
        {
          mark(m);

          while (not stack.empty())
          {
            auto [base, size_] = stack.back()->extent();

            stack.pop_back();

            size += size_;

            std::for_each(mutators.lower_bound(reinterpret_cast<mutator const*>(base)),
                          mutators.lower_bound(reinterpret_cast<mutator const*>(reinterpret_cast<std::uintptr_t>(base) + size_)),
                          mark);
          }
        }
      }

      for (auto object : objects)
      {
        if (not live_objects.contains(object))
        {
          delete object;
        }
      }

      objects.swap(live_objects);

      threshold = std::max(threshold, size + (size / 2));
    }

    static auto count() noexcept -> std::size_t
    {
      return objects.size();
    }

    static auto dlclose(void * const handle) -> void
    {
      if (handle and ::dlclose(handle))
      {
        std::cerr << ::dlerror() << std::endl;
      }
    }

    static auto dlopen(std::string const& filename) -> void *
    {
      ::dlerror(); // Clear

      try
      {
        return dynamic_linked_libraries.at(filename).get();
      }
      catch (std::out_of_range const&)
      {
        if (auto handle = ::dlopen(filename.c_str(), RTLD_LAZY | RTLD_GLOBAL); handle)
        {
          dynamic_linked_libraries.emplace(std::piecewise_construct,
                                           std::forward_as_tuple(filename),
                                           std::forward_as_tuple(handle, dlclose));

          return dlopen(filename);
        }
        else
        {
          throw std::runtime_error(::dlerror());
        }
      }
    }

    static auto dlsym(std::string const& symbol, void * const handle) -> void *
    {
      if (auto address = ::dlsym(handle, symbol.c_str()); address)
      {
        return address;
      }
      else
      {
        throw std::runtime_error(::dlerror());
      }
    }

    static auto is_root(mutator const* m) noexcept
    {
      /*
         If the given mutator is a non-root object, then an object containing
         this mutator as a data member exists somewhere in memory.

         Containing the mutator as a data member means that the address of the
         mutator is contained in the interval of the object's base-address ~
         base-address + object-size. The top is present to keep track of the
         base-address and size of the object needed here.
      */
      auto contains = [&](top const* object)
      {
        auto [base, size] = object->extent();
        return base <= m and m < reinterpret_cast<void const*>(reinterpret_cast<std::uintptr_t>(base) + size);
      };

      /*
         The memory layout of the base class Top and Bound of the binder is
         implementation-defined. That is, there is no guarantee that the
         pointer value of Top const* is less than the pointer value of Bound
         const*. Therefore, the iterator returned by lower_bound here points to
         top const*, which may be an iterator to the object itself, which may
         contain m, or the next iterator of the object, which may contain m.
      */
      auto next = objects.lower_bound(reinterpret_cast<top const*>(m));

      auto prev = std::prev(next);

      return not ((next and contains(*next)) or (prev and contains(*prev)));
    }
  };
} // namespace meevax::memory

#endif // INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP
