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

#ifndef INCLUDED_MEEVAX_MEMORY_GC_POINTER_HPP
#define INCLUDED_MEEVAX_MEMORY_GC_POINTER_HPP

#include <meevax/iostream/lexical_cast.hpp>
#include <meevax/memory/collector.hpp>
#include <meevax/memory/nan_boxing_pointer.hpp>
#include <meevax/type_traits/requires.hpp>
#include <meevax/utility/demangle.hpp>

namespace meevax
{
inline namespace memory
{
  template <typename Top, typename... Ts>
  struct gc_pointer : public nan_boxing_pointer<Top, Ts...>
                    , private collector::mutator
  {
    using base_pointer = nan_boxing_pointer<Top, Ts...>;

    gc_pointer(gc_pointer const& gcp)
      : base_pointer { gcp }
      , collector::mutator { gcp.object }
    {}

    gc_pointer(base_pointer const& p)
      : base_pointer { p }
      , collector::mutator { locate(base_pointer::get()) }
    {}

    gc_pointer(std::nullptr_t = nullptr)
    {}

    gc_pointer(Top * top) // TODO Top const*
      : base_pointer { top }
      , collector::mutator { locate(base_pointer::get()) }
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
      collector::mutator::reset(gcp.object);
    }

    auto reset(base_pointer const& p) -> void
    {
      base_pointer::reset(p);
      collector::mutator::reset(locate(base_pointer::get()));
    }

    auto reset(std::nullptr_t = nullptr) -> void
    {
      base_pointer::reset();
      collector::mutator::reset();
    }

    template <typename Bound, typename Allocator, typename... Us>
    static auto make(Us&&... xs) -> gc_pointer
    {
      if constexpr (std::is_same_v<Bound, Top>)
      {
        return collector::make<Top, Allocator>(std::forward<decltype(xs)>(xs)...);
      }
      else if constexpr (std::is_class_v<Bound>)
      {
        return collector::make<collector::binder<Top, Bound>, Allocator>(std::forward<decltype(xs)>(xs)...);
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
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_GC_POINTER_HPP
