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

#include <cstring>
#include <stdexcept>

#include <meevax/memory/model.hpp>

namespace meevax
{
inline namespace memory
{
  auto model::name() -> char const*
  {
    if (std::strcmp(value, lp32) == 0)
    {
      return "lp32";
    }
    else if (std::strcmp(value, ilp32) == 0)
    {
      return "ilp32";
    }
    else if (std::strcmp(value, llp64) == 0)
    {
      return "llp64";
    }
    else if (std::strcmp(value, lp64) == 0)
    {
      return "lp64";
    }
    else if (std::strcmp(value, ilp64) == 0)
    {
      return "ilp64";
    }
    else
    {
      throw std::logic_error("unknown C data model");
    }
  }
} // namespace memory
} // namespace meevax
