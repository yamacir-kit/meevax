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

#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace posix
{
  auto operator <<(std::ostream& port, const cursor_move& datum) -> decltype(port)
  {
    return escape_sequence(port, datum.value, datum.code);
  }
} // namespace posix
} // namespace meevax
