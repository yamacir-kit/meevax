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

#include <meevax/kernel/input_string_port.hpp>
#include <meevax/kernel/string.hpp>

namespace meevax
{
inline namespace kernel
{
  auto input_string_port::close() -> void
  {}

  auto input_string_port::is_open() const -> bool
  {
    return true;
  }

  auto input_string_port::istream() -> std::istream &
  {
    return stringstream;
  }

  auto input_string_port::istream() const -> std::istream const&
  {
    return stringstream;
  }

  auto operator <<(std::ostream & output, input_string_port const& datum) -> std::ostream &
  {
    return output << magenta("#,(") << blue("open-input-string ") << string(datum.stringstream.str()) << magenta(")");
  }

namespace literals
{
  auto operator ""_r(char const* s, std::size_t) -> object
  {
    return input_string_port(s).read();
  }
} // namespace literals
} // namespace kernel
} // namespace meevax
