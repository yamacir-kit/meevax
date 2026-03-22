/*
   Copyright 2018-2025 Tatsuya Yamasaki.

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

  auto pair::extent() const noexcept -> std::pair<void const*, std::size_t>
  {
    return { this, sizeof(*this) };
  }

  auto pair::equal1(pair const* other) const -> bool
  {
    return this == other;
  }

  auto pair::equal2(pair const* other) const -> bool
  {
    return other and *this == *other;
  }

  auto pair::type() const noexcept -> std::type_info const&
  {
    return typeid(pair);
  }

  auto pair::write(std::ostream & os) const -> std::ostream &
  {
    return os << *this;
  }

  struct datum_labels
  {
    std::ostream & os;

    auto static inline count_index = std::ios_base::xalloc();

    auto static inline table_index = std::ios_base::xalloc();

    using table = std::unordered_map<pair const*, std::size_t>;

    explicit datum_labels(std::ostream & os)
      : os { os }
    {
      if (not os.iword(count_index)++)
      {
        os.pword(table_index) = new table();
      }
    }

    ~datum_labels()
    {
      if (not --os.iword(count_index))
      {
        delete static_cast<table *>(std::exchange(os.pword(table_index), nullptr));
      }
    }

    auto static of(std::ostream & os)
    {
      return static_cast<table *>(os.pword(table_index));
    }
  };

  auto operator <<(std::ostream & os, pair const& datum) -> std::ostream &
  {
    auto _ = datum_labels(os);

    if (is_circular_list(cdr(datum)))
    {
      if (auto [iterator, success] = datum_labels::of(os)->emplace(&datum, datum_labels::of(os)->size() + 1); success)
      {
        os << magenta("#", iterator->second, "=(");

        for (auto&& x : datum)
        {
          os << x << " ";
        }

        return os << magenta(". #", iterator->second, "#)");
      }
      else
      {
        return os << magenta("#", iterator->second, "#");
      }
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
