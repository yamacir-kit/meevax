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

#include <meevax/kernel/binary_input_file_port.hpp>
#include <meevax/kernel/eof.hpp>
#include <meevax/kernel/homogeneous_vector.hpp>

namespace meevax::inline kernel
{
  auto binary_input_file_port::close() -> void
  {
    return ifstream.close();
  }

  auto binary_input_file_port::is_open() const -> bool
  {
    return ifstream.is_open();
  }

  auto binary_input_file_port::get() -> object
  {
    if (std::uint8_t buffer = 0; ifstream.read(reinterpret_cast<char *>(&buffer), 1))
    {
      return make(static_cast<small_integer>(buffer));
    }
    else
    {
      return eof_object;
    }
  }

  auto binary_input_file_port::get(std::size_t size) -> object
  {
    if (auto buffer = std::vector<std::uint8_t>(size); ifstream.read(reinterpret_cast<char *>(buffer.data()), size))
    {
      return make<u8vector>(direct_initialization, buffer.data(), buffer.size());
    }
    else
    {
      buffer.resize(ifstream.gcount());
      return make<u8vector>(direct_initialization, buffer.data(), buffer.size());
    }
  }

  auto binary_input_file_port::get_ready() const -> bool
  {
    return static_cast<bool>(ifstream);
  }

  auto binary_input_file_port::peek() -> object
  {
    if (get_ready())
    {
      return make<small_integer>(ifstream.peek());
    }
    else
    {
      return eof_object;
    }
  }

  auto operator <<(std::ostream & output, binary_input_file_port const& datum) -> std::ostream &
  {
    return output << magenta("#,(") << blue("open-binary-input-file ", datum.name) << magenta(")");
  }
} // namespace meevax::kernel
