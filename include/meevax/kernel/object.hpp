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
    using let = heterogeneous<cell, T>;

    virtual auto type() const noexcept -> std::type_info const&
    {
      return typeid(T);
    }

    virtual auto eqv(let const& x) const -> bool
    {
      if constexpr (is_equality_comparable<T>::value)
      {
        if (auto const* address = dynamic_cast<T const*>(x.get()); address)
        {
          return *address == static_cast<T const&>(*this);
        }
        else
        {
          return std::is_same<T, std::nullptr_t>::value;
        }
      }
      else
      {
        return false;
      }
    }

    virtual auto is_nan() const -> bool
    {
      return delay<nanp>().yield<bool>(static_cast<T const&>(*this));
    }

    virtual auto write_to(std::ostream & os) const -> std::ostream &
    {
      return delay<write>().yield<std::ostream &>(os, static_cast<T const&>(*this));
    }

    #define BOILERPLATE(SYMBOL, RESULT, FUNCTOR)                               \
    virtual auto operator SYMBOL(let const& x) const -> RESULT                 \
    {                                                                          \
      return delay<FUNCTOR>().yield<RESULT>(static_cast<T const&>(*this), x);  \
    } static_assert(true)

    BOILERPLATE(+, let, addition);
    BOILERPLATE(-, let, subtraction);
    BOILERPLATE(*, let, multiplication);
    BOILERPLATE(/, let, division);
    BOILERPLATE(%, let, modulo);

    BOILERPLATE(!=, bool, std::not_equal_to <void>);
    BOILERPLATE(<,  bool, std::less         <void>);
    BOILERPLATE(<=, bool, std::less_equal   <void>);
    BOILERPLATE(==, bool, std::equal_to     <void>);
    BOILERPLATE(>,  bool, std::greater      <void>);
    BOILERPLATE(>=, bool, std::greater_equal<void>);

    #undef BOILERPLATE

    #define DEFINE(NAME)                                                       \
    virtual auto NAME() const -> let                                           \
    {                                                                          \
      return delay<NAME##_t>().yield<let>(static_cast<T const&>(*this));       \
    } static_assert(true)

    DEFINE(exact);
    DEFINE(exp);
    DEFINE(inexact);
    DEFINE(log);
    DEFINE(sqrt);

    DEFINE(sin); DEFINE(asin); DEFINE(sinh); DEFINE(asinh);
    DEFINE(cos); DEFINE(acos); DEFINE(cosh); DEFINE(acosh);
    DEFINE(tan); DEFINE(atan); DEFINE(tanh); DEFINE(atanh);

    #undef DEFINE
  };

  template <typename T, typename... Ts>
  constexpr auto make(Ts&&... xs)
  {
    return let::allocate<T>(std::forward<decltype(xs)>(xs)...); // NOTE: This leaks memory if exception thrown from T's constructor.
  }

  template <typename T>
  constexpr auto make(T&& x)
  {
    return let::allocate<typename std::decay<T>::type>(std::forward<decltype(x)>(x));
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_OBJECT_HPP
