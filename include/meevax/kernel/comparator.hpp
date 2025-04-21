/*
   Copyright 2018-2024 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_KERNEL_COMPARATOR_HPP
#define INCLUDED_MEEVAX_KERNEL_COMPARATOR_HPP

#include <meevax/kernel/number.hpp>
#include <meevax/kernel/pair.hpp>

namespace meevax::inline kernel
{
  inline auto eq = [](auto const& x, auto const& y) constexpr
  {
    return x == y;
  };

  inline auto eqv = [](auto const& x, auto const& y)
  {
    return eq(x, y) or x.compare(y) or exact_integer_equals(x, y);
  };

  auto equal(object const&, object const&) -> bool;
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_COMPARATOR_HPP
