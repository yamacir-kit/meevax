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

#ifndef INCLUDED_MEEVAX_KERNEL_INTRINSIC_HPP
#define INCLUDED_MEEVAX_KERNEL_INTRINSIC_HPP

#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/syntax.hpp>

namespace meevax
{
inline namespace kernel
{
  struct intrinsic : public syntax
                   , public procedure
  {
    template <typename F, typename G>
    explicit intrinsic(symbol::value_type const& name, F&& f, G&& g)
      : syntax    { name, std::forward<decltype(f)>(f) }
      , procedure { name, std::forward<decltype(g)>(g) }
    {}
  };

  auto operator <<(std::ostream &, intrinsic const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_INTRINSIC_HPP
