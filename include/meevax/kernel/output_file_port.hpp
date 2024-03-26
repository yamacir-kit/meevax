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

#ifndef INCLUDED_MEEVAX_KERNEL_OUTPUT_FILE_PORT_HPP
#define INCLUDED_MEEVAX_KERNEL_OUTPUT_FILE_PORT_HPP

#include <fstream>

#include <meevax/kernel/textual_output_port.hpp>

namespace meevax
{
inline namespace kernel
{
  struct output_file_port : public textual_output_port
  {
    string const name;

    std::ofstream ofstream;

    template <typename T, typename... Ts>
    explicit output_file_port(T&& x, Ts&&... xs)
      : name { std::forward<decltype(x)>(x) }
      , ofstream { name, std::forward<decltype(xs)>(xs)... }
    {}

    auto close() -> void override;

    auto is_open() const -> bool override;

    operator std::ostream &() override;
  };

  auto operator <<(std::ostream &, output_file_port const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_OUTPUT_FILE_PORT_HPP
