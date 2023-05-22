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

#ifndef INCLUDED_MEEVAX_KERNEL_INPUT_FILE_PORT_HPP
#define INCLUDED_MEEVAX_KERNEL_INPUT_FILE_PORT_HPP

#include <fstream>

#include <meevax/kernel/textual_input_port.hpp>

namespace meevax
{
inline namespace kernel
{
  struct input_file_port : public textual_input_port
  {
    string const name;

    std::ifstream ifstream;

    template <typename T, typename... Ts>
    explicit input_file_port(T&& x, Ts&&... xs)
      : name { std::forward<decltype(x)>(x) }
      , ifstream { name, std::forward<decltype(xs)>(xs)... }
    {}

    auto close() -> void override;

    auto is_open() const -> bool override;

    operator std::istream &() override;

    operator std::istream const&() const override;
  };

  auto operator <<(std::ostream &, input_file_port const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_INPUT_FILE_PORT_HPP
