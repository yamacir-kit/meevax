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

#include <meevax/functional/compose.hpp>
#include <meevax/iostream/escape_sequence.hpp>
#include <meevax/iostream/write.hpp>
#include <meevax/kernel/overview.hpp>
#include <meevax/kernel/profiler.hpp>
#include <meevax/type_traits/is_equality_comparable.hpp>
#include <meevax/utility/module.hpp>

namespace meevax
{
inline namespace kernel
{
  template <template <typename...> typename Pointer, typename Top, typename... Ts>
  class heterogeneous : public Pointer<Top, Ts...>
  {
    template <typename Bound>
    struct binder : public virtual Top
                  , public Bound
    {
      template <typename... Us>
      explicit constexpr binder(Us&&... xs)
        : std::conditional<std::is_base_of<Top, Bound>::value, Top, Bound>::type { std::forward<decltype(xs)>(xs)... }
      {}

      ~binder() override = default;

      auto compare([[maybe_unused]] Top const* top) const -> bool override
      {
        if constexpr (is_equality_comparable<Bound>::value)
        {
          if (auto const* bound = dynamic_cast<Bound const*>(top); bound)
          {
            return *bound == static_cast<Bound const&>(*this);
          }
          else
          {
            return std::is_same<Bound, null>::value;
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
        return os << static_cast<Bound const&>(*this);
      }
    };

  public:
    using Pointer<Top, Ts...>::Pointer;
    using Pointer<Top, Ts...>::dereferenceable;
    using Pointer<Top, Ts...>::get;

    template <typename Bound, typename... Us>
    static auto allocate(Us&&... xs)
    {
      #if PROFILE_ALLOCATION
      current_profiler().by_type[typeid(typename std::decay<Bound>::type)].allocation++;
      current_profiler().by_type[typeid(void)].allocation++;
      #endif

      if constexpr (std::is_same_v<Bound, Top>)
      {
        return heterogeneous(new (gc) Top(std::forward<decltype(xs)>(xs)...));
      }
      else if constexpr (std::is_class_v<Bound>)
      {
        return heterogeneous(new (gc) binder<Bound>(std::forward<decltype(xs)>(xs)...));
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
        if (auto data = dynamic_cast<U *>(get()); data)
        {
          return *data;
        }
        else
        {
          std::stringstream ss {};
          ss << "no viable conversion from " << demangle(type()) << " to " << demangle(typeid(U));
          raise(ss.str());
        }
      }
      else
      {
        return Pointer<Top, Ts...>::template as<U>();
      }
    }

    template <typename U>
    inline auto as_const() const -> U const&
    {
      return as<const U>();
    }

    inline auto compare(heterogeneous const& rhs) const -> bool
    {
      if (dereferenceable())
      {
        return *this ? get()->compare(rhs.get()) : not rhs;
      }
      else
      {
        return Pointer<Top, Ts...>::equivalent_to(rhs);
      }
    }

    template <typename U>
    inline auto is() const
    {
      return type() == typeid(typename std::decay<U>::type);
    }

    template <typename U, REQUIRES(std::is_class<U>)>
    inline auto is_also() const
    {
      return dynamic_cast<U *>(get()) != nullptr;
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

    friend auto operator <<(std::ostream & os, heterogeneous const& datum) -> std::ostream &
    {
      if (datum.dereferenceable())
      {
        return not datum ? os << magenta("()") : datum->write(os);
      }
      else
      {
        return datum.write(os);
      }
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_POINTER_HPP
