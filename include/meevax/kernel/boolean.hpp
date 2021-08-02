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

#ifndef INCLUDED_MEEVAX_KERNEL_BOOLEAN_HPP
#define INCLUDED_MEEVAX_KERNEL_BOOLEAN_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax
{
inline namespace kernel
{
  struct boolean
  {
    const bool value;

    constexpr boolean(bool value)
      : value { value }
    {}

    constexpr operator bool() const noexcept
    {
      return value;
    }
  };

  auto operator <<(std::ostream &, boolean const&) -> std::ostream &;

  extern let const t;
  extern let const f;

  auto if_(let const& x) -> bool;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_BOOLEAN_HPP
