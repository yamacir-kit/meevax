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

#include <meevax/kernel/binary_output_file_port.hpp>

namespace meevax::inline kernel
{
  auto binary_output_file_port::close() -> void
  {
    ofstream.close();
  }

  auto binary_output_file_port::flush() -> void
  {
    ofstream << std::flush;
  }

  auto binary_output_file_port::is_open() const -> bool
  {
    return ofstream.is_open();
  }

  auto binary_output_file_port::put(std::uint8_t u8) -> void
  {
    ofstream.write(reinterpret_cast<char const*>(&u8), 1);
  }

  auto binary_output_file_port::put(u8vector const& v) -> void
  {
    for (auto u8 : v.valarray())
    {
      ofstream.write(reinterpret_cast<char const*>(&u8), 1);
    }
  }

  auto operator <<(std::ostream & output, binary_output_file_port const& datum) -> std::ostream &
  {
    return output << magenta("#,(") << blue("open-binary-output-file ", datum.name) << magenta(")");
  }
} // namespace meevax::kernel
