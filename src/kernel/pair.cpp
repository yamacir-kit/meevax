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

#include <meevax/kernel/list.hpp>

namespace meevax::inline kernel
{
  let unit = nullptr;

  auto pair::equal1(top const* other) const -> bool
  {
    assert(false);
    return this == dynamic_cast<pair const*>(other);
  }

  auto pair::equal2(top const* other) const -> bool
  {
    assert(false);
    auto pare = dynamic_cast<pair const*>(other);
    return pare and *this == *pare;
  }

  auto pair::type() const noexcept -> std::type_info const&
  {
    assert(false);
    return typeid(pair);
  }

  auto pair::write(std::ostream & os) const -> std::ostream &
  {
    return os << *this;
  }

  auto operator <<(std::ostream & os, pair const& datum) -> std::ostream &
  {
    if (is_circular_list(cdr(datum)))
    {
      auto n = reinterpret_cast<std::uintptr_t>(&datum);

      os << magenta("#", n, "=(");

      for (auto&& x : datum)
      {
        os << x << " ";
      }

      return os << magenta(". #", n, "#)");
    }
    else
    {
      os << magenta("(") << car(datum);

      for (let xs = cdr(datum); not xs.is<null>(); xs = cdr(xs))
      {
        if (xs.is<pair>())
        {
          os << " " << car(xs);
        }
        else // xs is the last element of dotted-list.
        {
          return os << magenta(" . ") << xs << magenta(")");
        }
      }

      return os << magenta(")");
    }
  }
} // namespace meevax::kernel
