/*
   Copyright 2018-2025 Tatsuya Yamasaki.

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

#include <meevax/kernel/input_file_port.hpp>

namespace meevax::inline kernel
{
  auto input_file_port::close() -> void
  {
    return ifstream.close();
  }

  auto input_file_port::cons(object const& x, object const& y) -> object
  {
    return textual_context::cons(x, y, name);
  }

  auto input_file_port::is_open() const -> bool
  {
    return ifstream.is_open();
  }

  auto input_file_port::istream() -> std::istream &
  {
    return ifstream;
  }

  auto input_file_port::istream() const -> std::istream const&
  {
    return ifstream;
  }

  auto operator <<(std::ostream & output, input_file_port const& datum) -> std::ostream &
  {
    return output << magenta("#,(") << blue("open-input-file ") << string(datum.name) << magenta(")");
  }
} // namespace meevax::kernel
