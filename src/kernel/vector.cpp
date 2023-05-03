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

#include <numeric>

#include <meevax/kernel/error.hpp>
#include <meevax/kernel/exact_integer.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/vector.hpp>

namespace meevax
{
inline namespace kernel
{
  vector::vector(object const& x)
  {
    std::copy(std::begin(x), std::end(x), std::back_inserter(objects));
  }

  vector::vector(std::size_t k, object const& fill)
    : objects { k, fill }
  {}

  auto vector::operator [](std::size_t k) const -> object const&
  {
    return objects[k];
  }

  auto operator ==(vector const& lhs, vector const& rhs) -> bool
  {
    return std::equal(std::begin(lhs.objects), std::end(lhs.objects),
                      std::begin(rhs.objects), std::end(rhs.objects), equal);
  }

  auto operator <<(std::ostream & output, vector const& datum) -> std::ostream &
  {
    output << magenta("#(");

    auto whitespace = "";

    for (auto const& each : datum.objects)
    {
      output << std::exchange(whitespace, " ") << each;
    }

    return output << magenta(")");
  }
} // namespace kernel
} // namespace meevax
