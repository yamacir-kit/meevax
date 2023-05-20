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

#include <meevax/kernel/standard_output_port.hpp>

namespace meevax
{
inline namespace kernel
{
  auto standard_output_port::close() -> void
  {}

  auto standard_output_port::is_open() const -> bool
  {
    return true;
  }

  standard_output_port::operator std::ostream &()
  {
    return std::cout;
  }

  auto operator <<(std::ostream & output, standard_output_port const&) -> std::ostream &
  {
    return output << magenta("#,(") << blue("standard-output") << magenta(")");
  }
} // namespace kernel
} // namespace meevax
