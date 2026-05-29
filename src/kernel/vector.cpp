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

#include <numeric>

#include <meevax/kernel/comparator.hpp>
#include <meevax/kernel/vector.hpp>

namespace meevax::inline kernel
{
  auto operator ==(heterogeneous_vector const& v, heterogeneous_vector const& u) -> bool
  {
    return std::equal(v.objects.begin(), v.objects.end(), u.objects.begin(), u.objects.end(), equal);
  }

  auto operator <<(std::ostream & output, heterogeneous_vector const& datum) -> std::ostream &
  {
    output << magenta("#(");

    auto whitespace = "";

    for (auto const& object : datum.objects)
    {
      output << std::exchange(whitespace, " ") << object;
    }

    return output << magenta(")");
  }
} // namespace meevax::kernel
