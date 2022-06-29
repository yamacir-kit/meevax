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

#include <meevax/algorithm/for_each.hpp>
#include <meevax/kernel/error.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/vector.hpp>

namespace meevax
{
inline namespace kernel
{
  auto vector::fill(const_reference x, size_type from, size_type to) -> void
  {
    for (auto iter = std::next(std::begin(data), from); iter != std::next(std::begin(data), to); ++iter)
    {
      *iter = x;
    }
  }

  auto vector::list(size_type from, size_type to) const -> value_type
  {
    let x = unit;

    for (auto iter = std::prev(std::rend(data), to); iter != std::prev(std::rend(data), from); ++iter)
    {
      x = cons(*iter, x);
    }

    return x;
  }

  auto vector::list(size_type from) const -> value_type
  {
    return list(from, std::size(data));
  }

  auto vector::string(size_type from, size_type to) const -> value_type
  {
    meevax::string s;

    for (auto iter = std::prev(std::rend(data), to); iter != std::prev(std::rend(data), from); ++iter)
    {
      if ((*iter).is<character>())
      {
        s.push_back((*iter).as<character>());
      }
      else
      {
        throw error(make<meevax::string>("It is an error if any element of vector between start and end is not a character"), unit);
      }
    }

    return make(s);
  }

  auto vector::string(size_type from) const -> value_type
  {
    return string(from, std::size(data));
  }

  auto operator ==(vector const& lhs, vector const& rhs) -> bool
  {
    return std::equal(std::begin(lhs.data), std::end(lhs.data),
                      std::begin(rhs.data), std::end(rhs.data), equal);
  }

  auto operator <<(std::ostream & os, vector const& datum) -> std::ostream &
  {
    return os << magenta("#(") << for_each(datum.data) << magenta(")");
  }
} // namespace kernel
} // namespace meevax
