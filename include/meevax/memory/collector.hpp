/*
   Copyright 2018-2023 Tatsuya Yamasaki.

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

#if __unix__
#include <dlfcn.h> // dlopen, dlclose, dlerror
#else
#error
#endif

#include <memory> // std::allocator
#include <unordered_map>

#include <meevax/iostream/escape_sequence.hpp>
#include <meevax/iostream/lexical_cast.hpp>
#include <meevax/memory/integer_set.hpp>
#include <meevax/memory/literal.hpp>
#include <meevax/memory/nan_boxing_pointer.hpp>
#include <meevax/memory/simple_allocator.hpp>
#include <meevax/type_traits/is_equality_comparable.hpp>
#include <meevax/type_traits/is_output_streamable.hpp>
#include <meevax/utility/demangle.hpp>

namespace meevax
{
inline namespace memory
{
  template <typename... Ts>
  using default_allocator = std::allocator<Ts...>;

  /*
     This mark-and-sweep garbage collector is based on the implementation of
     gc_ptr written by William E. Kempf and posted to CodeProject.

     - https://www.codeproject.com/Articles/912/A-garbage-collection-framework-for-C
     - https://www.codeproject.com/Articles/938/A-garbage-collection-framework-for-C-Part-II
  */
  template <typename Top, typename... Ts>
  class collector
  {
  public:
    struct top
    {
      virtual ~top() = default;

      virtual auto compare(top const*) const -> bool = 0;

      virtual auto type() const noexcept -> std::type_info const& = 0;

      virtual auto write(std::ostream &) const -> std::ostream & = 0;

      virtual auto lower() const noexcept -> std::uintptr_t = 0;

      virtual auto upper() const noexcept -> std::uintptr_t = 0;

      auto contains(void const* const data) const noexcept
      {
        return reinterpret_cast<void const*>(lower()) <= data and data < reinterpret_cast<void const*>(upper());
      }
    };

    static inline auto cleared = false;

    template <typename Bound, typename AllocatorTraits = std::allocator_traits<std::allocator<void>>>
    struct binder : public virtual std::conditional_t<std::is_same_v<Top, Bound>, top, Top>
                  , public Bound
    {
      struct allocator_type : public AllocatorTraits::template rebind_alloc<binder<Bound, AllocatorTraits>>
      {
        ~allocator_type()
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

      static inline auto allocator = allocator_type();

      template <typename... Us>
      explicit constexpr binder(Us&&... xs)
        : std::conditional_t<std::is_base_of_v<Top, Bound> and std::is_constructible_v<Top, Us...>, Top, Bound> {
            std::forward<decltype(xs)>(xs)...
          }
      {}

      ~binder() override = default;

      auto compare([[maybe_unused]] top const* other) const -> bool override
      {
        if constexpr (is_equality_comparable_v<Bound const&>)
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

      auto lower() const noexcept -> std::uintptr_t override
      {
        return reinterpret_cast<std::uintptr_t>(this);
      }

      auto upper() const noexcept -> std::uintptr_t override
      {
        return reinterpret_cast<std::uintptr_t>(this) + sizeof(*this);
      }

      auto operator new(std::size_t) -> void *
      {
        return allocator.allocate(1);
      }

      auto operator delete(void * data) noexcept -> void
      {
        using pointer = typename std::allocator_traits<allocator_type>::pointer;
        allocator.deallocate(reinterpret_cast<pointer>(data), 1);
      }
    };

    class mutator
    {
      friend class collector;

    protected:
      top const* object = nullptr;

      mutator() = default;

      explicit mutator(top const* object) noexcept
        : object { object }
      {
        if (object)
        {
          mutators.insert(this);
        }
      }

      ~mutator() noexcept
      {
        mutators.erase(this);
      }

      auto reset(top const* after = nullptr) noexcept -> void
      {
        if (object = after; after)
        {
          mutators.insert(this);
        }
        else
        {
          mutators.erase(this);
        }
      }
    };

    struct gc_pointer : public nan_boxing_pointer<Top, Ts...>
                      , private collector<Top, Ts...>::mutator
    {
      using base_pointer = nan_boxing_pointer<Top, Ts...>;

      gc_pointer(gc_pointer const& gcp)
        : base_pointer { gcp }
        , collector<Top, Ts...>::mutator { gcp.object }
      {}

      gc_pointer(base_pointer const& p)
        : base_pointer { p }
        , collector<Top, Ts...>::mutator { base_pointer::get() }
      {}

      gc_pointer(std::nullptr_t = nullptr)
      {}

      gc_pointer(Top * top) // TODO Top const*
        : base_pointer { top }
        , collector<Top, Ts...>::mutator { base_pointer::get() }
      {}

      template <typename T, typename = std::enable_if_t<(std::is_same_v<T, Ts> or ... or std::is_same_v<T, double>)>>
      gc_pointer(T const& datum)
        : base_pointer { datum }
      {
        assert(base_pointer::get() == nullptr);
      }

      auto operator =(gc_pointer const& gcp) -> auto &
      {
        reset(gcp);
        return *this;
      }

      auto operator =(base_pointer const& p) -> auto &
      {
        reset(p);
        return *this;
      }

      auto operator =(std::nullptr_t) -> auto &
      {
        reset();
        return *this;
      }

      auto reset(gc_pointer const& gcp) -> void
      {
        base_pointer::reset(gcp);
        collector<Top, Ts...>::mutator::reset(gcp.object);
      }

      auto reset(base_pointer const& p) -> void
      {
        base_pointer::reset(p);
        collector<Top, Ts...>::mutator::reset(base_pointer::get());
      }

      auto reset(std::nullptr_t = nullptr) -> void
      {
        base_pointer::reset();
        collector<Top, Ts...>::mutator::reset();
      }

      template <typename Bound, typename Allocator, typename... Us>
      static auto make(Us&&... xs) -> gc_pointer
      {
        if constexpr (std::is_class_v<Bound>)
        {
          return collector<Top, Ts...>::template make<
                   typename collector<Top, Ts...>::template binder<
                     Bound, std::allocator_traits<Allocator>>>(
                       std::forward<decltype(xs)>(xs)...
                       );
        }
        else
        {
          return { std::forward<decltype(xs)>(xs)... };
        }
      }

      template <typename U>
      inline auto as() const -> decltype(auto)
      {
        if constexpr (std::is_same_v<std::decay_t<U>, Top>)
        {
          return base_pointer::operator *();
        }
        else if constexpr (std::is_class_v<std::decay_t<U>>)
        {
          if (auto data = dynamic_cast<std::add_pointer_t<U>>(base_pointer::get()); data)
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
          return base_pointer::template as<U>();
        }
      }

      template <typename U>
      inline auto as() -> decltype(auto)
      {
        if constexpr (std::is_same_v<std::decay_t<U>, Top>)
        {
          return base_pointer::operator *();
        }
        else if constexpr (std::is_class_v<std::decay_t<U>>)
        {
          if (auto data = dynamic_cast<std::add_pointer_t<U>>(base_pointer::get()); data)
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
          return base_pointer::template as<U>();
        }
      }

      template <typename U>
      inline auto as_const() const -> decltype(auto)
      {
        return as<std::add_const_t<U>>();
      }

      inline auto compare(gc_pointer const& rhs) const -> bool
      {
        if (base_pointer::dereferenceable())
        {
          return *this ? base_pointer::get()->compare(rhs.get()) : rhs.is<std::nullptr_t>();
        }
        else
        {
          return base_pointer::compare(rhs);
        }
      }

      template <typename U>
      inline auto is() const
      {
        return type() == typeid(std::decay_t<U>);
      }

      template <typename U, REQUIRES(std::is_class<U>)>
      inline auto is_also() const
      {
        return dynamic_cast<std::add_pointer_t<U>>(base_pointer::get()) != nullptr;
      }

      inline auto type() const -> std::type_info const&
      {
        if (base_pointer::dereferenceable())
        {
          return *this ? base_pointer::get()->type() : typeid(std::nullptr_t);
        }
        else
        {
          return base_pointer::type();
        }
      }

      inline auto write(std::ostream & os) const -> std::ostream &
      {
        if (base_pointer::dereferenceable())
        {
          return *this ? base_pointer::get()->write(os) : os << magenta("()");
        }
        else
        {
          return base_pointer::write(os);
        }
      }

      friend auto operator <<(std::ostream & os, gc_pointer const& datum) -> std::ostream &
      {
        return datum.write(os);
      }

      inline auto begin()
      {
        return *this ? base_pointer::get()->begin() : typename Top::iterator();
      }

      inline auto begin() const
      {
        return *this ? base_pointer::get()->cbegin() : typename Top::const_iterator();
      }

      inline auto cbegin() const
      {
        return *this ? base_pointer::get()->cbegin() : typename Top::const_iterator();
      }

      inline auto end()
      {
        return *this ? base_pointer::get()->end() : typename Top::iterator();
      }

      inline auto end() const
      {
        return *this ? base_pointer::get()->cend() : typename Top::const_iterator();
      }

      inline auto cend() const
      {
        return *this ? base_pointer::get()->cend() : typename Top::const_iterator();
      }
    };

    /*
       0x0000'0000'0000'0000 ~ 0x7FFF'FFFF'FFFF'FFFF
    */
    template <typename T>
    using pointer_set = integer_set<T const*, 15, 16, 16>;

  private:
    static inline pointer_set<top> objects {};

    static inline pointer_set<mutator> mutators {};

    static inline std::size_t allocation = 0;

    static inline std::size_t threshold = 8_MiB;

    static inline std::unordered_map<std::string, std::unique_ptr<void, void (*)(void * const)>> dynamic_linked_libraries {};

  public:
    collector() = delete;

    collector(collector &&) = delete;

    collector(collector const&) = delete;

    ~collector() = delete;

    auto operator =(collector &&) -> collector & = delete;

    auto operator =(collector const&) -> collector & = delete;

    template <typename T, typename... Us>
    static auto make(Us&&... xs)
    {
      static_assert(std::is_base_of_v<top, T>);

      if (allocation += sizeof(T); threshold < allocation)
      {
        collect();
      }

      if (auto data = new T(std::forward<decltype(xs)>(xs)...); data)
      {
        objects.insert(data);

        return data;
      }
      else
      {
        throw std::bad_alloc();
      }
    }

    static auto clear() -> void
    {
      for (auto&& object : objects)
      {
        delete object;
        objects.erase(object);
      }
    }

    static auto collect() -> void
    {
      allocation = 0;

      return sweep(mark());
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

    static auto mark() noexcept -> pointer_set<top>
    {
      auto is_root_object = [begin = objects.begin()](mutator const* given) // TODO INEFFICIENT!
      {
        /*
           If the given mutator is a non-root object, then an object containing
           this mutator as a data member exists somewhere in memory.

           Containing the mutator as a data member means that the address of
           the mutator is contained in the interval of the object's
           base-address ~ base-address + object-size. The top is present to
           keep track of the base-address and size of the object needed here.
        */
        auto iter = objects.lower_bound(reinterpret_cast<top const*>(given));

        return iter == begin or not (*--iter)->contains(given);
      };

      auto marked_objects = pointer_set<top>();

      for (auto&& mutator : mutators)
      {
        assert(mutator);
        assert(mutator->object);

        if (not marked_objects.contains(mutator->object) and is_root_object(mutator))
        {
          mark(mutator->object, marked_objects);
        }
      }

      return marked_objects;
    }

    static auto mark(top const* const object, pointer_set<top> & marked_objects) noexcept -> void
    {
      assert(object);

      assert(objects.contains(object));

      if (not marked_objects.contains(object))
      {
        marked_objects.insert(object);

        auto lower = mutators.lower_bound(reinterpret_cast<mutator const*>(object->lower()));
        auto upper = mutators.lower_bound(reinterpret_cast<mutator const*>(object->upper()));

        for (; lower != upper; ++lower)
        {
          mark((*lower)->object, marked_objects);
        }
      }
    }

    static auto sweep(pointer_set<top> && marked_objects) -> void
    {
      for (auto marked_object : marked_objects)
      {
        objects.erase(marked_object);
      }

      for (auto object : objects)
      {
        delete object;
      }

      objects.swap(marked_objects);
    }
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP
