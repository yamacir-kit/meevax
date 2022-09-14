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

#include <numeric>

#include <meevax/algorithm/for_each.hpp>
#include <meevax/kernel/error.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/vector.hpp>

namespace meevax
{
inline namespace kernel
{
  vector::vector(const_reference x)
  {
    std::copy(std::begin(x), std::end(x), std::back_inserter(data));
  }

  vector::vector(string const& s)
  {
    for (auto const& c : s.codepoints)
    {
      data.push_back(make(c));
    }
  }

  vector::vector(const_reference k, const_reference fill)
    : data { k.as<exact_integer>(), fill }
  {}

  auto vector::append(const_reference xs) -> value_type
  {
    let const v = make<vector>();

    for (let const& x : xs)
    {
      std::copy(x.as<vector>().data.begin(),
                x.as<vector>().data.end(),
                std::back_inserter(v.as<vector>().data));
    }

    return v;
  }

  auto vector::copy(const_reference from, const_reference to) const -> value_type
  {
    let const& v = make<vector>();

    std::copy(std::next(std::begin(data), from.as<exact_integer>()),
              std::next(std::begin(data), to.as<exact_integer>()),
              std::back_inserter(v.as<vector>().data));

    return v;
  }

  auto vector::copy(const_reference at, const_reference v, const_reference from, const_reference to) -> void
  {
    data.reserve(data.size() + v.as<vector>().data.size());

    std::copy(std::next(std::begin(v.as<vector>().data), from.as<exact_integer>()),
              std::next(std::begin(v.as<vector>().data), to.as<exact_integer>()),
              std::next(std::begin(data), at.as<exact_integer>()));
  }

  auto vector::fill(const_reference x, const_reference from, const_reference to) -> void
  {
    std::fill(std::next(std::begin(data), from.as<exact_integer>()),
              std::next(std::begin(data), to.as<exact_integer>()),
              x);
  }

  auto vector::length() const -> value_type
  {
    return make<exact_integer>(data.size());
  }

  auto vector::list(const_reference from, const_reference to) const -> value_type
  {
    return std::accumulate(std::prev(std::rend(data), to.as<exact_integer>()),
                           std::prev(std::rend(data), from.as<exact_integer>()),
                           unit,
                           xcons);
  }

  auto vector::ref(const_reference k) const -> const_reference
  {
    return data.at(k.as<exact_integer>());
  }

  auto vector::set(const_reference k, const_reference x) -> const_reference
  {
    return data.at(k.as<exact_integer>()) = x;
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
