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
  heterogeneous_vector::heterogeneous_vector(object const& x)
  {
    std::copy(std::begin(x), std::end(x), std::back_inserter(vector));
  }

  heterogeneous_vector::heterogeneous_vector(std::size_t size, object const& x)
    : vector { size, x }
  {}

  auto heterogeneous_vector::operator [](std::size_t index) const -> object const&
  {
    return vector[index];
  }

  auto heterogeneous_vector::operator [](std::size_t index) -> object &
  {
    return vector[index];
  }

  auto operator ==(heterogeneous_vector const& v, heterogeneous_vector const& u) -> bool
  {
    return std::equal(std::begin(v.vector), std::end(v.vector),
                      std::begin(u.vector), std::end(u.vector), equal);
  }

  auto operator <<(std::ostream & output, heterogeneous_vector const& datum) -> std::ostream &
  {
    output << magenta("#(");

    auto whitespace = "";

    for (auto const& each : datum.vector)
    {
      output << std::exchange(whitespace, " ") << each;
    }

    return output << magenta(")");
  }
} // namespace kernel
} // namespace meevax
