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

#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  let unit { nullptr };

  pair::pair(const_reference a, const_reference b)
    : std::pair<value_type, value_type> { a, b }
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

  auto label(const_reference x, const_reference y) -> const_reference
  {
    if (x.is<pair>() and cdr(x).is<pair>() and (cddr(x) == cdr(y) or label(cddr(x), cdr(y))))
    {
      return cdddr(x);
    }
    else
    {
      return unit;
    }
  }

  auto label(pair const& x)
  {
    if (cdr(x).is<pair>() and (cddr(x) == cdr(x) or label(cddr(x), cdr(x))))
    {
      return cdddr(x);
    }
    else
    {
      return unit;
    }
  }

  auto operator <<(std::ostream & os, pair const& datum) -> std::ostream &
  {
    if (let const& end = label(datum))
    {
      auto n = reinterpret_cast<std::uintptr_t>(end.get());

      os << magenta("#", n, "=(") << car(datum);

      for (auto iter = std::begin(cdr(datum)); iter != end; ++iter)
      {
        os << " " << *iter;
      }

      return os << magenta(" . #", n, "#)");
    }
    else
    {
      os << magenta("(") << car(datum);

      for (auto iter = std::begin(cdr(datum)); iter != end; ++iter)
      {
        if (iter.get().is<pair>())
        {
          os << " " << *iter;
        }
        else // iter is the last element of dotted-list.
        {
          return os << magenta(" . ") << iter.get() << magenta(")");
        }
      }

      return os << magenta(")");
    }
  }
} // namespace kernel
} // namespace meevax
