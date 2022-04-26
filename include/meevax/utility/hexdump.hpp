/*
   Copyright 2018-2022 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_UTILITY_HEXDUMP
#define INCLUDED_MEEVAX_UTILITY_HEXDUMP

#include <iomanip> // std::hex,
#include <iostream> // std::ostream
#include <vector> // std::vector

namespace meevax
{
inline namespace utility
{
  template <typename T>
  struct hexdump
  {
    const std::vector<std::uint8_t> data;

    explicit hexdump(const T& value)
      : data {
          reinterpret_cast<typename decltype(data)::const_pointer>(&value),
          reinterpret_cast<typename decltype(data)::const_pointer>(&value) + sizeof(T)
        }
    {}

    // TODO UPDATE WITH STD::ENDIAN (C++20)
    auto operator()(std::ostream & os) const -> std::ostream &
    {
      for (auto iter = std::rbegin(data); iter != std::rend(data); ++iter) // little endian
      // for (auto iter = std::begin(data); iter != std::end(data); ++iter) // big endian
      {
        os << std::setw(2) << std::setfill('0') << std::hex << static_cast<unsigned int>(*iter) << " ";
      }

      return os;
    }
  };

  template <typename T>
  auto operator <<(std::ostream & os, hexdump<T> const& hexdump) -> std::ostream &
  {
    return hexdump(os);
  }
} // namespace utility
} // namespace meevax

#endif // INCLUDED_MEEVAX_UTILITY_HEXDUMP
