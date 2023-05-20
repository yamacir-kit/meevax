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

#include <meevax/kernel/output_string_port.hpp>
#include <meevax/kernel/string.hpp>

namespace meevax
{
inline namespace kernel
{
  auto output_string_port::close() -> void
  {}

  auto output_string_port::is_open() const -> bool
  {
    return true;
  }

  output_string_port::operator std::ostream &()
  {
    return ostringstream;
  }

  auto operator <<(std::ostream & output, output_string_port const& datum) -> std::ostream &
  {
    return output << magenta("#,(") << blue("open-output-string ") << string(datum.ostringstream.str()) << magenta(")");
  }
} // namespace kernel
} // namespace meevax
