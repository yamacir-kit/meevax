/*
   Copyright 2018-2023 Tatsuya Yamasaki.

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

#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/identity.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax
{
inline namespace kernel
{
  auto absolute::load() const -> object const&
  {
    if (second.is<absolute>()) // Only the (export (rename ...)) form makes an identity whose value is identity.
    {
      assert(second.is<absolute>());
      return second.as<absolute>().load();
    }
    else
    {
      return second;
    }
  }

  auto absolute::store(object const& x) -> void
  {
    second = x;
  }

  auto absolute::symbol() const -> object const&
  {
    assert(first.is_also<identifier>());
    return first;
  }

  auto operator <<(std::ostream & os, absolute const& datum) -> std::ostream &
  {
    if (datum.load() == undefined)
    {
      return os << faint(datum.symbol());
    }
    else
    {
      return os << blue(datum.symbol());
    }
  }
} // namespace kernel
} // namespace meevax
