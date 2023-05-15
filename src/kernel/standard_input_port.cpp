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

#include <meevax/kernel/standard_input_port.hpp>

namespace meevax
{
inline namespace kernel
{
  standard_input_port::operator std::istream &()
  {
    return std::cin;
  }

  standard_input_port::operator std::istream const&() const
  {
    return std::cin;
  }

  auto operator <<(std::ostream & output, standard_input_port const&) -> std::ostream &
  {
    return output << magenta("#,(") << blue("standard-input") << magenta(")");
  }
} // namespace kernel
} // namespace meevax