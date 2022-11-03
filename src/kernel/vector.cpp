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
#include <meevax/kernel/exact_integer.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/vector.hpp>

namespace meevax
{
inline namespace kernel
{
  vector::vector(const_reference x)
  {
    std::copy(std::begin(x), std::end(x), std::back_inserter(objects));
  }

  vector::vector(string const& s)
  {
    for (auto const& c : s.codepoints)
    {
      objects.push_back(make(c));
    }
  }

  vector::vector(const_reference k, const_reference fill)
    : objects { k.as<exact_integer>(), fill }
  {}

  auto vector::fill(const_reference x, const_reference from, const_reference to) -> void
  {
    std::fill(std::next(std::begin(objects), from.as<exact_integer>()),
              std::next(std::begin(objects), to.as<exact_integer>()),
              x);
  }

  auto vector::length() const -> value_type
  {
    return make<exact_integer>(objects.size());
  }

  auto vector::list(const_reference from, const_reference to) const -> value_type
  {
    return std::accumulate(std::prev(std::rend(objects), to.as<exact_integer>()),
                           std::prev(std::rend(objects), from.as<exact_integer>()),
                           unit,
                           xcons);
  }

  auto vector::operator [](std::size_t k) const -> const_reference
  {
    return objects[k];
  }

  auto operator ==(vector const& lhs, vector const& rhs) -> bool
  {
    return std::equal(std::begin(lhs.objects), std::end(lhs.objects),
                      std::begin(rhs.objects), std::end(rhs.objects), equal);
  }

  auto operator <<(std::ostream & os, vector const& datum) -> std::ostream &
  {
    return os << magenta("#(") << for_each(datum.objects) << magenta(")");
  }
} // namespace kernel
} // namespace meevax
