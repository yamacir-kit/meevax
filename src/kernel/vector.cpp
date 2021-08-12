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

#include <boost/range/adaptors.hpp>
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

  auto vector::fill(let const& value, size_type from, size_type to) -> void
  {
    using boost::adaptors::sliced;

    for (auto&& each : *this | sliced(from, to))
    {
      each = value;
    }
  }

  auto vector::fill(let const& value, size_type from) -> void
  {
    fill(value, from, size());
  }

  auto vector::fill(let const& value, let const& from) -> void
  {
    fill(value, static_cast<size_type>(from.as<exact_integer>()));
  }

  auto vector::fill(let const& value, let const& from, let const& to) -> void
  {
    fill(value, static_cast<size_type>(from.as<exact_integer>()), static_cast<size_type>(to.as<exact_integer>()));
  }

  auto vector::to_list(size_type from, size_type to) const -> value_type
  {
    using boost::adaptors::reversed;
    using boost::adaptors::sliced;

    let x = unit;

    for (let const& each : *this | sliced(from, to) | reversed)
    {
      push(x, each);
    }

    return x;
  }

  auto vector::to_string(size_type from, size_type to) const -> value_type
  {
    using boost::adaptors::sliced;

    string s;

    for (let const& each : *this | sliced(from, to))
    {
      if (each.is<character>())
      {
        s.push_back(each.as<character>());
      }
      else
      {
        throw error(make<string>("It is an error if any element of vector between start and end is not a character"), unit);
      }
    }

    return make(s);
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
