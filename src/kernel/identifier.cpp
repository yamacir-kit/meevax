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

#include <meevax/kernel/identifier.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  auto identifier::assq() const -> let const&
  {
    return kernel::assq(unwrap_syntax(), global_environment());
  }

  auto identifier::global_environment() const noexcept -> let const&
  {
    return std::get<1>(*this);
  }

  auto identifier::is_bound() const -> bool
  {
    return not is_free();
  }

  auto identifier::is_free() const -> bool
  {
    return assq().eqv(f);
  }

  auto identifier::lookup() const -> let const&
  {
    if (let const& x = assq(); x != f)
    {
      return cdr(x);
    }
    else
    {
      return unwrap_syntax();
    }
  }

  auto identifier::unwrap_syntax() const noexcept -> let const&
  {
    return std::get<0>(*this);
  }

  auto operator <<(std::ostream & os, identifier const& datum) -> std::ostream &
  {
    return os << underline << datum.unwrap_syntax() << reset;
  }

  auto lookup(let const& x, let const& g) -> let const&
  {
    if (let const& p = assq(x, g); p != f)
    {
      return cdr(p);
    }
    else
    {
      return x.is<identifier>() ? x.as<identifier>().lookup() : x;
    }
  }
} // namespace kernel
} // namespace meevax
