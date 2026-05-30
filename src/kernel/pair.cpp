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

#include <meevax/kernel/circular_list.hpp>
#include <meevax/kernel/list.hpp>

namespace meevax::inline kernel
{
  auto pair::eqv(pair const* x) const -> bool
  {
    return static_cast<pair const*>(this) == x and static_cast<pair const&>(*this) == *x;
  }

  auto pair::extent() const noexcept -> std::pair<void const*, void const*>
  {
    auto const base = dynamic_cast<void const*>(this);
    return { base, reinterpret_cast<void const*>(reinterpret_cast<std::uintptr_t>(base) + extent_) };
  }

  auto pair::type() const noexcept -> std::type_info const&
  {
    return typeid(pair);
  }

  auto pair::write(std::ostream & o) const -> std::ostream &
  {
    return o << static_cast<pair const&>(*this);
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

    if (is_circular_list(datum.second))
    {
      if (auto [iterator, success] = datum_labels::of(os)->emplace(&datum, datum_labels::of(os)->size() + 1); success)
      {
        os << magenta("#", iterator->second, "=(");

        for (auto&& x : datum | as_circular_list)
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
      os << magenta("(") << datum.first;

      for (let xs = datum.second; not xs.is<null>(); xs = cdr(xs))
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
