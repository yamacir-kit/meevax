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

#include <meevax/kernel/input_string_port.hpp>
#include <meevax/kernel/string.hpp>

namespace meevax
{
inline namespace kernel
{
  input_string_port::operator std::istream &()
  {
    return istringstream;
  }

  input_string_port::operator std::istream const&() const
  {
    return istringstream;
  }

  auto operator <<(std::ostream & output, input_string_port const& datum) -> std::ostream &
  {
    return output << magenta("#,(") << blue("open-input-string ") << string(datum.istringstream.str()) << magenta(")");
  }
} // namespace kernel
} // namespace meevax
