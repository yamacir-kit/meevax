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

namespace meevax
{
inline namespace kernel
{
  struct syntactic_context
  {
    using value_type = std::uint64_t;

    const value_type value;

    explicit constexpr syntactic_context(value_type value = 0)
      : value { value }
    {}

    template <typename... Ts>
    constexpr auto operator [](Ts&&... xs) const noexcept
    {
      return in(std::forward<decltype(xs)>(xs)...);
    }

    constexpr auto operator |(syntactic_context const& c) const noexcept
    {
      return syntactic_context(value | c.value);
    }

    constexpr auto is_in(syntactic_context c) const noexcept -> bool
    {
      return value & c.value;
    }
  };

  namespace context
  {
    constexpr auto free = syntactic_context();

    constexpr auto outermost = syntactic_context(1 << 0);

    constexpr auto tail_call = syntactic_context(1 << 1);
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTEXT_HPP
