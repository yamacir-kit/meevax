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

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/equivalence.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator <<(std::ostream & os, boolean const& datum) -> std::ostream &
  {
    return os << cyan << "#" << std::boolalpha << datum.value << reset;
  }

  let const t = make<boolean>(true);
  let const f = make<boolean>(false);

  auto if_(const_reference x) -> bool
  {
    return not eq(x, f) or not eqv(x, f);
  }
} // namespace kernel
} // namespace meevax
