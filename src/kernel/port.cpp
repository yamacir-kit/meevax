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

#include <ios>

#include <meevax/kernel/port.hpp>
#include <meevax/kernel/reader.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator <<(std::ostream & output, standard_input_port const&) -> std::ostream &
  {
    return output << magenta("#,(") << blue("standard-input-port") << magenta(")");
  }

  auto operator <<(std::ostream & output, standard_output_port const&) -> std::ostream &
  {
    return output << magenta("#,(") << blue("standard-output-port") << magenta(")");
  }

  auto operator <<(std::ostream & output, standard_error_port const&) -> std::ostream &
  {
    return output << magenta("#,(") << blue("standard-error-port") << magenta(")");
  }

  auto operator <<(std::ostream & output, file_port const& datum) -> std::ostream &
  {
    return output << magenta("#,(") << blue("open ") << datum.name << magenta(")");
  }

  auto operator <<(std::ostream & output, string_port const& datum) -> std::ostream &
  {
    return output << magenta("#,(") << blue("string->port ") << string(datum.stringstream.str()) << magenta(")");
  }
} // namespace kernel
} // namespace meevax
