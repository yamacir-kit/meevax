/*
   Copyright 2018-2021 Tatsuya Yamasaki.

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
#include <meevax/kernel/profiler.hpp>
#include <meevax/type_traits/delay.hpp>
#include <meevax/type_traits/is_equality_comparable.hpp>
#include <meevax/utility/module.hpp>

namespace meevax
{
inline namespace kernel
{
  template <template <typename...> typename Pointer, typename Top>
  class heterogeneous : public Pointer<Top>
  {
    template <typename Bound>
    struct binder : public virtual Top
                  , public Bound
    {
      template <typename... Ts>
      explicit constexpr binder(Ts&&... xs)
        : std::conditional<std::is_base_of<Top, Bound>::value, Top, Bound>::type { std::forward<decltype(xs)>(xs)... }
      {}

      ~binder() override = default;

      auto compare(heterogeneous const& x) const -> bool override
      {
        if constexpr (is_equality_comparable<Bound>::value)
        {
          if (auto const* address = dynamic_cast<Bound const*>(x.get()); address)
          {
            return *address == static_cast<Bound const&>(*this);
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
        return delay<iostream::write>().yield<decltype(os)>(os, static_cast<Bound const&>(*this));
      }
    };

  public:
    using Pointer<Top>::Pointer;
    using Pointer<Top>::get;

    template <typename Bound, typename... Ts, REQUIRES(std::is_compound<Bound>)>
    static auto allocate(Ts&&... xs)
    {
      #if PROFILE_ALLOCATION
      current_profiler()[typeid(typename std::decay<Bound>::type)].allocation++;
      #endif

      if constexpr (std::is_same<Bound, Top>::value)
      {
        return static_cast<heterogeneous>(new (gc) Top(std::forward<decltype(xs)>(xs)...));
      }
      else
      {
        return static_cast<heterogeneous>(new (gc) binder<Bound>(std::forward<decltype(xs)>(xs)...));
      }
    }

    template <typename U>
    inline auto as() const -> U &
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

    template <typename U>
    inline auto as_const() const -> U const&
    {
      return as<const U>();
    }

    inline auto compare(heterogeneous const& rhs) const -> bool
    {
      return type() == rhs.type() and get()->compare(rhs);
    }

    template <typename U>
    inline auto is() const
    {
      if constexpr (std::is_null_pointer<typename std::decay<U>::type>::value)
      {
        return not static_cast<bool>(*this);
      }
      else
      {
        return type() == typeid(typename std::decay<U>::type);
      }
    }

    template <typename U>
    inline auto is_also() const
    {
      return dynamic_cast<U *>(get()) != nullptr;
    }

    inline auto type() const -> std::type_info const&
    {
      return *this ? get()->type() : typeid(null);
    }
  };

  template <template <typename...> typename Pointer, typename Top>
  auto operator <<(std::ostream & os, heterogeneous<Pointer, Top> const& datum) -> std::ostream &
  {
    return datum.template is<null>() ? os << magenta("()") : datum->write(os);
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_POINTER_HPP
