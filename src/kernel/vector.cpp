/*
   Copyright 2018-2024 Tatsuya Yamasaki.

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
#include <meevax/kernel/large_integer.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/vector.hpp>

namespace meevax::inline kernel
{
  auto operator ==(heterogeneous_vector const& v, heterogeneous_vector const& u) -> bool
  {
    return std::equal(v.begin(), v.end(),
                      u.begin(), u.end(), equal);
  }

  auto operator <<(std::ostream & output, heterogeneous_vector const& datum) -> std::ostream &
  {
    output << magenta("#(");

    auto whitespace = "";

    for (auto const& each : datum)
    {
      output << std::exchange(whitespace, " ") << each;
    }

    return output << magenta(")");
  }

  auto make_vector(object const& xs) -> object
  {
    return make<vector>(xs.begin(), xs.end());
  }
} // namespace meevax::kernel
