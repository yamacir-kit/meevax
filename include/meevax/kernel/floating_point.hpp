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

#ifndef INCLUDED_MEEVAX_KERNEL_FLOATING_POINT_HPP
#define INCLUDED_MEEVAX_KERNEL_FLOATING_POINT_HPP

#include <cmath>

#include <meevax/iostream/lexical_cast.hpp>
#include <meevax/kernel/error.hpp>
#include <meevax/kernel/ratio.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename T>
  struct floating_point
  {
    T value;

    explicit constexpr floating_point(T value = {})
      : value { value }
    {}

    constexpr operator T() const noexcept { return value; }
    constexpr operator T()       noexcept { return value; }
  };

  template <typename T>
  auto operator <<(std::ostream & os, floating_point<T> const& rhs) -> std::ostream &
  {
    return os << make(rhs.value);
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_FLOATING_POINT_HPP
