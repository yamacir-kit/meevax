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

#ifndef INCLUDED_MEEVAX_MEMORY_LITERAL_HPP
#define INCLUDED_MEEVAX_MEMORY_LITERAL_HPP

namespace meevax
{
inline namespace memory
{
  constexpr auto operator ""_KiB(unsigned long long size)
  {
    return size * 1024;
  }

  constexpr auto operator ""_MiB(unsigned long long size)
  {
    return size * 1024 * 1024;
  }
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_LITERAL_HPP
