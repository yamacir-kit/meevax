/*
   Copyright 2018-2024 Tatsuya Yamasaki.

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
#include <unordered_map>

#include <meevax/iostream/escape_sequence.hpp>
#include <meevax/iostream/lexical_cast.hpp>
#include <meevax/memory/integer_set.hpp>
#include <meevax/memory/literal.hpp>
#include <meevax/memory/nan_boxing_pointer.hpp>
#include <meevax/type_traits/is_equality_comparable.hpp>
#include <meevax/type_traits/is_output_streamable.hpp>
#include <meevax/utility/demangle.hpp>

namespace meevax
{
inline namespace memory
{
  using view = std::pair<void const*, std::size_t>; // TODO Adapt to C++20's std::range concept

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

      virtual auto view() const noexcept -> memory::view = 0;

      auto contains(void const* const data) const noexcept
      {
        auto [address, size] = view();
        return address <= data and data < static_cast<std::byte const*>(address) + size;
      }
    };

    static inline auto cleared = false;

    template <typename Bound, typename AllocatorTraits>
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

      auto view() const noexcept -> memory::view override
      {
        return { this, sizeof(*this) };
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
          mutators.insert(this);
        }
      }

      mutator(Top * top)
        : pointer { top }
      {
        if (top)
        {
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
        if (not cleared)
        {
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

      auto reset(mutator const& other) -> void
      {
        if (pointer::reset(other); other)
        {
          mutators.insert(this);
        }
        else
        {
          mutators.erase(this);
        }
      }

      auto reset(std::nullptr_t = nullptr) -> void
      {
        pointer::reset();
        mutators.erase(this);
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

      inline auto compare(mutator const& rhs) const -> bool
      {
        if (pointer::dereferenceable())
        {
          return *this ? pointer::get()->compare(rhs.get()) : rhs.is<std::nullptr_t>();
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
          return *this ? pointer::get()->type() : typeid(std::nullptr_t);
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
          return *this ? pointer::get()->write(os) : os << magenta("()");
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

      inline auto begin()
      {
        return *this ? pointer::get()->begin() : typename Top::iterator();
      }

      inline auto begin() const
      {
        return *this ? pointer::get()->cbegin() : typename Top::const_iterator();
      }

      inline auto cbegin() const
      {
        return *this ? pointer::get()->cbegin() : typename Top::const_iterator();
      }

      inline auto end()
      {
        return *this ? pointer::get()->end() : typename Top::iterator();
      }

      inline auto end() const
      {
        return *this ? pointer::get()->cend() : typename Top::const_iterator();
      }

      inline auto cend() const
      {
        return *this ? pointer::get()->cend() : typename Top::const_iterator();
      }
    };

    /*
       https://www.kernel.org/doc/html/latest/arch/x86/x86_64/mm.html

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

    struct mutators_view
    {
      std::uintptr_t address;

      std::size_t size;

      constexpr mutators_view(view const& v)
        : address { reinterpret_cast<std::uintptr_t>(v.first) }
        , size    { v.second }
      {}

      auto begin() const noexcept
      {
        return mutators.lower_bound(reinterpret_cast<mutator const*>(address));
      }

      auto end() const noexcept
      {
        return mutators.lower_bound(reinterpret_cast<mutator const*>(address + size));
      }
    };

  public:
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
        if (allocation += sizeof(T); threshold < allocation)
        {
          collect();
        }

        if (auto data = new binder<T, std::allocator_traits<Allocator>>(std::forward<decltype(xs)>(xs)...); data)
        {
          objects.insert(data);

          return data;
        }
        else
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
        assert(mutator->get());

        if (not marked_objects.contains(mutator->get()) and is_root_object(mutator))
        {
          mark(mutator->get(), marked_objects);
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

        for (auto each : mutators_view(object->view()))
        {
          mark(each->get(), marked_objects);
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
