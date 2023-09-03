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

#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  let unit { nullptr };

  pair::pair(object const& a, object const& b)
    : std::pair<object, object> { a, b }
  {}

  auto pair::compare(pair const* that) const -> bool
  {
    return that and *this == *that;
  }

  auto pair::type() const noexcept -> std::type_info const&
  {
    return typeid(pair);
  }

  auto pair::write(std::ostream & os) const -> std::ostream &
  {
    return os << *this;
  }

  auto pair::operator [](std::size_t k) const -> object const&
  {
    return 0 < k ? second[--k] : first;
  }

  auto operator <<(std::ostream & os, pair const& datum) -> std::ostream &
  {
    auto is_circular_list = [&]()
    {
      for (auto rest = datum.second.get(); rest; rest = rest->second.get())
      {
        if (rest == &datum)
        {
          return true;
        }
      }

      return false;
    };

    if (is_circular_list())
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

      for (let xs = cdr(datum); xs != unit; xs = cdr(xs))
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
} // namespace kernel
} // namespace meevax
