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

#ifndef INCLUDED_MEEVAX_KERNEL_CONTEXT_HPP
#define INCLUDED_MEEVAX_KERNEL_CONTEXT_HPP

namespace meevax
{
inline namespace kernel
{
  struct context
  {
    enum value_type
    {
      none,
      outermost = (1 << 0),
      tail      = (1 << 1),
      size,
    }
    const value;

    template <typename T>
    constexpr context(T const value) noexcept
      : value { static_cast<value_type>(value) }
    {}

    constexpr operator value_type() const noexcept
    {
      return value;
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_CONTEXT_HPP