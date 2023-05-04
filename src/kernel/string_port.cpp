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

#include <meevax/kernel/string_port.hpp>

namespace meevax
{
inline namespace kernel
{
  string_port::operator std::istream &()
  {
    return stringstream;
  }

  string_port::operator std::ostream &()
  {
    return stringstream;
  }

  string_port::operator std::string()
  {
    return stringstream.str();
  }

  auto operator <<(std::ostream & output, string_port const& datum) -> std::ostream &
  {
    return output << magenta("#,(") << blue("string->port ") << string(datum.stringstream.str()) << magenta(")");
  }
} // namespace kernel
} // namespace meevax
