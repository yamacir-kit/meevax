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

#ifndef INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTEXT_HPP
#define INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTEXT_HPP

#include <cstdint>

#include <meevax/type_traits/underlying_cast.hpp>

namespace meevax
{
inline namespace kernel
{
  enum class syntactic_context : std::uint64_t
  {
    none,

    outermost = (1 << 0),

    tail_call = (1 << 1),
  };

  constexpr auto operator |(syntactic_context const c1, syntactic_context const c2) noexcept
  {
    return static_cast<syntactic_context>(underlying_cast(c1) | underlying_cast(c2));
  }

  constexpr auto operator &(syntactic_context const c1, syntactic_context const c2) noexcept
  {
    return static_cast<syntactic_context>(underlying_cast(c1) & underlying_cast(c2));
  }

  constexpr auto operator &&(syntactic_context const c1, syntactic_context const c2) noexcept
  {
    return static_cast<bool>(underlying_cast(c1) & underlying_cast(c2));
  }

  using context = syntactic_context;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTEXT_HPP
