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

  auto find_circulation(object const& x, object const& y) -> object const&
  {
    if (x.is<pair>() and cdr(x).is<pair>() and (cddr(x) == cdr(y) or find_circulation(cddr(x), cdr(y))))
    {
      return cdddr(x);
    }
    else
    {
      return unit;
    }
  }

  auto find_circulation(pair const& x)
  {
    if (cdr(x).is<pair>() and (cddr(x) == cdr(x) or find_circulation(cddr(x), cdr(x))))
    {
      return cdddr(x);
    }
    else
    {
      return unit;
    }
  }

  auto pair::end() -> iterator
  {
    return iterator(find_circulation(*this).get());
  }

  auto pair::end() const -> const_iterator
  {
    return const_iterator(find_circulation(*this).get());
  }

  auto pair::cend() const -> const_iterator
  {
    return std::as_const(*this).end();
  }

  auto write_simple(std::ostream & os, pair const& datum) -> std::ostream &
  {
    write_simple(os << magenta("("), car(datum));

    for (let xs = cdr(datum); xs != unit; xs = cdr(xs))
    {
      if (xs.is<pair>())
      {
        write_simple(os << " ", car(xs));
      }
      else // xs is the last element of dotted-list.
      {
        return write_simple(os << magenta(" . "), xs) << magenta(")");
      }
    }

    return os << magenta(")");
  }

  auto write_simple(std::ostream & os, object const& x) -> std::ostream &
  {
    return x.is<pair>() ? write_simple(os, x.as<pair>()) : os << x;
  }

  auto operator <<(std::ostream & os, pair const& datum) -> std::ostream &
  {
    if (auto end = datum.cend(); end != pair::const_iterator())
    {
      auto n = reinterpret_cast<std::uintptr_t>(end.pare);

      auto iter = datum.cbegin();

      os << magenta("#", n, "=(") << *iter;

      while (++iter != end)
      {
        os << " " << *iter;
      }

      return os << magenta(" . #", n, "#)");
    }
    else
    {
      return write_simple(os, datum);
    }
  }
} // namespace kernel
} // namespace meevax
