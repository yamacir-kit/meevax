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

#include <meevax/kernel/continuation.hpp>

namespace meevax
{
inline namespace kernel
{
  auto continuation::s() const -> const_reference { return   car(*this); }
  auto continuation::e() const -> const_reference { return  cadr(*this); }
  auto continuation::c() const -> const_reference { return caddr(*this); }
  auto continuation::d() const -> const_reference { return cdddr(*this); }

  auto operator <<(std::ostream & os, continuation const& datum) -> std::ostream &
  {
    return os << magenta("#,(") << green("continuation ") << faint(";#", std::addressof(datum)) << magenta(")");
  }
} // namespace kernel
} // namespace meevax
