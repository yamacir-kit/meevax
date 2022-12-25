/*
   Copyright 2018-2022 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_KERNEL_POINTER_HPP
#define INCLUDED_MEEVAX_KERNEL_POINTER_HPP

#include <meevax/functional/combinator.hpp>
#include <meevax/functional/compose.hpp>
#include <meevax/iostream/escape_sequence.hpp>
#include <meevax/iostream/lexical_cast.hpp>
#include <meevax/kernel/profiler.hpp>
#include <meevax/kernel/type_index.hpp>
#include <meevax/memory/gc_pointer.hpp>
#include <meevax/type_traits/is_array_subscriptable.hpp>
#include <meevax/type_traits/is_equality_comparable.hpp>
#include <meevax/type_traits/is_output_streamable.hpp>
#include <meevax/utility/demangle.hpp>

namespace meevax
{
inline namespace kernel
{
  using null = std::nullptr_t;

  template <template <typename...> typename Pointer, typename Top, typename... Ts>
  class heterogeneous : public Pointer<Top, Ts...>
  {
    template <typename Bound>
    struct binder : public virtual Top
                  , public Bound
    {
      template <typename... Us>
      explicit constexpr binder(Us&&... xs)
        : std::conditional_t<std::is_base_of_v<Top, Bound>, Top, Bound> { std::forward<decltype(xs)>(xs)... }
      {}

      ~binder() override = default;

      auto compare([[maybe_unused]] Top const* top) const -> bool override
      {
        if constexpr (is_equality_comparable_v<Bound>)
        {
          if (auto const* bound = dynamic_cast<Bound const*>(top); bound)
          {
            return *bound == static_cast<Bound const&>(*this);
          }
          else
          {
            return std::is_same_v<Bound, null>;
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
        if constexpr (is_output_streamable_v<Bound>)
        {
          return os << static_cast<Bound const&>(*this);
        }
        else
        {
          return os << magenta("#,(") << green(typeid(Bound).name()) << faint(" #;", static_cast<Bound const*>(this)) << magenta(")");
        }
      }

      auto operator []([[maybe_unused]] std::size_t k) const -> heterogeneous const& override
      {
        if constexpr (is_array_subscriptable_v<Bound>)
        {
          return static_cast<Bound const&>(*this)[k];
        }
        else
        {
          throw std::runtime_error(lexical_cast<std::string>("no viable array subscript operator for ", demangle(type())));
        }
      }
    };

  public:
    using Pointer<Top, Ts...>::Pointer;
    using Pointer<Top, Ts...>::dereferenceable;
    using Pointer<Top, Ts...>::get;

    template <typename Bound, typename... Us>
    static auto allocate(Us&&... xs)
    {
      if constexpr (profiler::count_allocations)
      {
        current_profiler().allocation_counts[typeid(Bound)]++;
        current_profiler().allocation_counts[typeid(void)]++;
      }

      if constexpr (std::is_same_v<Bound, Top>)
      {
        return heterogeneous(gc.make<Top>(std::forward<decltype(xs)>(xs)...));
      }
      else if constexpr (std::is_class_v<Bound>)
      {
        return heterogeneous(gc.make<binder<Bound>>(std::forward<decltype(xs)>(xs)...));
      }
      else
      {
        return heterogeneous(std::forward<decltype(xs)>(xs)...);
      }
    }

    template <typename U>
    inline auto as() const -> decltype(auto)
    {
      if constexpr (std::is_class_v<U>)
      {
        if (auto data = dynamic_cast<std::add_pointer_t<U>>(get()); data)
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
        return Pointer<Top, Ts...>::template as<U>();
      }
    }

    template <typename U>
    inline auto as_const() const -> decltype(auto)
    {
      return as<std::add_const_t<U>>();
    }

    inline auto compare(heterogeneous const& rhs) const -> bool
    {
      if (dereferenceable())
      {
        return *this ? get()->compare(rhs.get()) : rhs.is<null>();
      }
      else
      {
        return Pointer<Top, Ts...>::compare(rhs);
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
      return dynamic_cast<std::add_pointer_t<U>>(get()) != nullptr;
    }

    inline auto type() const -> std::type_info const&
    {
      if (dereferenceable())
      {
        return *this ? get()->type() : typeid(null);
      }
      else
      {
        return Pointer<Top, Ts...>::type();
      }
    }

    inline auto write(std::ostream & os) const -> std::ostream &
    {
      if (dereferenceable())
      {
        return *this ? get()->write(os) : os << magenta("()");
      }
      else
      {
        return Pointer<Top, Ts...>::write(os);
      }
    }

    inline auto operator [](std::size_t k) const -> heterogeneous const&
    {
      if (dereferenceable())
      {
        return *this ? get()->operator [](k) : *this; // throw std::runtime_error(lexical_cast<std::string>("no viable array subscript operator for ", demangle(type())));
      }
      else
      {
        throw std::runtime_error(lexical_cast<std::string>("no viable array subscript operator for ", demangle(type())));
      }
    }

    friend auto operator <<(std::ostream & os, heterogeneous const& datum) -> std::ostream &
    {
      return datum.write(os);
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_POINTER_HPP
