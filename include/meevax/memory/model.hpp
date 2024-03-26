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

#ifndef INCLUDED_MEEVAX_MEMORY_MODEL_HPP
#define INCLUDED_MEEVAX_MEMORY_MODEL_HPP

namespace meevax
{
inline namespace memory
{
  struct model
  {
    static constexpr char  lp32[] { 1, 2, 2, 4, 4, 0 };
    static constexpr char ilp32[] { 1, 2, 4, 4, 4, 0 };
    static constexpr char llp64[] { 1, 2, 4, 4, 8, 0 };
    static constexpr char  lp64[] { 1, 2, 4, 8, 8, 0 };
    static constexpr char ilp64[] { 1, 2, 8, 8, 8, 0 };

    static constexpr char value[] {
      sizeof(char), sizeof(short), sizeof(int), sizeof(long), sizeof(void *), 0
    };

    static auto name() -> char const*;
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_MODEL_HPP
