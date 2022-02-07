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

#include <meevax/string/header.hpp>

namespace meevax
{
  auto header(std::string const& from, std::size_t size) -> std::string
  {
    std::string s = "; ";

    s.append(from);
    s.resize(size + 2, ' ');
    s.replace(s.size() - 3, 3, " ; ");

    return s;
  }
} // namespace meevax
