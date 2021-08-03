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

#ifndef INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP
#define INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP

#ifndef MEEVAX_USE_GMP
#define MEEVAX_USE_GMP
#endif

#ifdef MEEVAX_USE_GMP
#include <boost/multiprecision/gmp.hpp>
#else
#include <boost/multiprecision/cpp_int.hpp>
#endif

#include <meevax/kernel/numeric_tower.hpp>

namespace meevax
{
inline namespace kernel
{
  struct exact_integer
  {
    #ifdef MEEVAX_USE_GMP
    using value_type = boost::multiprecision::mpz_int;
    #else
    using value_type = boost::multiprecision::cpp_int;
    #endif

    value_type value;

    template <typename... Ts>
    explicit constexpr exact_integer(Ts&&... xs)
      : value { std::forward<decltype(xs)>(xs)... }
    {}

    static constexpr std::true_type is_integer {};

    template <typename T>
    auto to() const
    {
      return value.convert_to<T>();
    }

    auto to_string() const
    {
      return value.str();
    }

    auto as_exact() const noexcept -> decltype(auto)
    {
      return *this;
    }

    template <typename T, REQUIRES(std::is_floating_point<T>)>
    auto as_inexact() const
    {
      return floating_point(to<T>());
    }

    operator value_type() const noexcept { return value; }
    operator value_type()       noexcept { return value; }
  };

  // Extremely frequently used exact-integer values.
  let extern const e0, e1;

  template <typename T, REQUIRES(std::is_integral<T>)> auto operator ==(exact_integer const& a, T&& b) { return a.value == b; }
  template <typename T, REQUIRES(std::is_integral<T>)> auto operator !=(exact_integer const& a, T&& b) { return a.value != b; }
  template <typename T, REQUIRES(std::is_integral<T>)> auto operator < (exact_integer const& a, T&& b) { return a.value <  b; }
  template <typename T, REQUIRES(std::is_integral<T>)> auto operator <=(exact_integer const& a, T&& b) { return a.value <= b; }
  template <typename T, REQUIRES(std::is_integral<T>)> auto operator > (exact_integer const& a, T&& b) { return a.value >  b; }
  template <typename T, REQUIRES(std::is_integral<T>)> auto operator >=(exact_integer const& a, T&& b) { return a.value >= b; }

  auto operator <<(std::ostream &, exact_integer const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP
