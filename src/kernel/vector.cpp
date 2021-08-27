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

#include <meevax/algorithm/for_each.hpp>
#include <meevax/kernel/error.hpp>
#include <meevax/kernel/stack.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/vector.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  vector::vector(for_each_in_tag, let const& xs)
    : vector { for_each_in, std::cbegin(xs), std::cend(xs) }
  {}

  auto vector::fill(const_reference x, size_type from, size_type to) -> void
  {
    for (auto iter = std::next(begin(), from); iter != std::next(begin(), to); ++iter)
    {
      *iter = x;
    }
  }

  auto vector::fill(const_reference x, size_type from) -> void
  {
    fill(x, from, size());
  }

  auto vector::list(size_type from, size_type to) const -> value_type
  {
    let x = unit;

    for (auto iter = std::prev(rend(), to); iter != std::prev(rend(), from); ++iter)
    {
      x = cons(*iter, x);
    }

    return x;
  }

  auto vector::list(size_type from) const -> value_type
  {
    return list(from, size());
  }

  auto vector::string(size_type from, size_type to) const -> value_type
  {
    meevax::string s;

    for (auto iter = std::prev(rend(), to); iter != std::prev(rend(), from); ++iter)
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
    return string(from, size());
  }

  auto operator ==(vector const& lhs, vector const& rhs) -> bool
  {
    return std::equal(std::begin(lhs), std::end(lhs),
                      std::begin(rhs), std::end(rhs), equal);
  }

  auto operator <<(std::ostream & os, vector const& datum) -> std::ostream &
  {
    return os << magenta << "#(" << reset << for_each(datum) << magenta << ")" << reset;
  }
} // namespace kernel
} // namespace meevax
