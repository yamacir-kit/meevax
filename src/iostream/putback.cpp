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

#include <iterator>

#include <meevax/iostream/putback.hpp>

namespace meevax
{
inline namespace iostream
{
  auto putback(std::istream & is, std::string const& s) -> std::istream &
  {
    for (auto iter = std::crbegin(s); iter != std::crend(s); ++iter)
    {
      is.putback(*iter);
    }

    return is;
  }
} // namespace iostream
} // namespace meevax
