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

#ifndef INCLUDED_MEEVAX_KERNEL_OBJECT_HPP
#define INCLUDED_MEEVAX_KERNEL_OBJECT_HPP

#include <meevax/kernel/heterogeneous.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename T>
  struct top
  {
    virtual auto compare(T const* p) const -> bool
    {
      if constexpr (is_equality_comparable<T>::value)
      {
        if (p)
        {
          return *p == static_cast<T const&>(*this);
        }
        else
        {
          return std::is_same<T, null>::value;
        }
      }
      else
      {
        return false;
      }
    }

    virtual auto type() const noexcept -> std::type_info const&
    {
      return typeid(T);
    }

    virtual auto write(std::ostream & os) const -> std::ostream &
    {
      return os << static_cast<T const&>(*this);
    }
  };

  template <typename T, typename... Ts>
  constexpr auto make(Ts&&... xs)
  {
    return xvalue::allocate<T>(std::forward<decltype(xs)>(xs)...); // NOTE: This leaks memory if exception thrown from T's constructor.
  }

  template <typename T>
  constexpr auto make(T&& x)
  {
    return xvalue::allocate<typename std::decay<T>::type>(std::forward<decltype(x)>(x));
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_OBJECT_HPP
