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

#include <meevax/kernel/standard_input_port.hpp>

namespace meevax::inline kernel
{
  standard_input_port::standard_input_port()
  {
    enable_source_cons("/dev/stdin");
  }

  auto standard_input_port::is_open() const -> bool
  {
    return true;
  }

  auto standard_input_port::close() -> void
  {}

  auto standard_input_port::istream() -> std::istream &
  {
    return std::cin;
  }

  auto standard_input_port::istream() const -> std::istream const&
  {
    return std::cin;
  }

  auto operator <<(std::ostream & output, standard_input_port const&) -> std::ostream &
  {
    return output << magenta("#,(") << blue("standard-input") << magenta(")");
  }
} // namespace meevax::kernel
