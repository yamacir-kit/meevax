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

#include <bitset>

namespace meevax
{
inline namespace kernel
{
  struct syntactic_context
  {
    std::bitset<2> data;

    template <typename... Ts>
    explicit constexpr syntactic_context(Ts&&... xs)
      : data { std::forward<decltype(xs)>(xs)... }
    {}

    auto at_the_top_level() const
    {
      return data.test(0);
    }

    auto in_a_tail_context() const
    {
      return data.test(1);
    }

    auto take_over(syntactic_context const& context) -> decltype(auto)
    {
      data |= context.data;
      return *this;
    }

    auto take_over(syntactic_context const& context) const
    {
      syntactic_context result { data | context.data };
      return result;
    }
  };

  constexpr syntactic_context in_context_free   { static_cast<std::uint64_t>(0x00) };
  constexpr syntactic_context at_the_top_level  { static_cast<std::uint64_t>(0x01) };
  constexpr syntactic_context in_a_tail_context { static_cast<std::uint64_t>(0x02) };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTEXT_HPP
