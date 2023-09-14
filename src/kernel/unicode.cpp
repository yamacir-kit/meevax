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

#include <meevax/kernel/unicode.hpp>

extern char _binary_UnicodeData_txt_start[];
extern char _binary_UnicodeData_txt_size[];

namespace meevax
{
inline namespace kernel
{
  auto unicode_data() -> std::string_view
  {
    return {_binary_UnicodeData_txt_start, reinterpret_cast<std::uintptr_t>(_binary_UnicodeData_txt_size)};
  }
} // namespace kernel
} // namespace meevax
