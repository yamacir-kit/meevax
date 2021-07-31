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

#include <meevax/string/indent.hpp>

namespace meevax
{
  auto operator <<(std::ostream & os, indent const& datum) -> std::ostream &
  {
    return os << static_cast<std::string>(datum);
  }

  auto operator <<(indent & datum, std::size_t width) -> indent &
  {
    indent::depth -= std::min(indent::depth, width);
    return datum;
  }

  auto operator <<(indent && datum, std::size_t width) -> indent &
  {
    indent::depth -= std::min(indent::depth, width);
    return datum;
  }

  auto operator >>(indent & datum, std::size_t width) -> indent &
  {
    indent::depth += width;
    return datum;
  }

  auto operator >>(indent && datum, std::size_t width) -> indent &
  {
    indent::depth += width;
    return datum;
  }
} // namespace meevax
