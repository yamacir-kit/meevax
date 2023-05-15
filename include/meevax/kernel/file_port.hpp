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

#ifndef INCLUDED_MEEVAX_KERNEL_FILE_PORT_HPP
#define INCLUDED_MEEVAX_KERNEL_FILE_PORT_HPP

#include <fstream>

#include <meevax/kernel/textual_input_port.hpp>
#include <meevax/kernel/textual_output_port.hpp>

namespace meevax
{
inline namespace kernel
{
  struct file_port : public textual_input_port, public textual_output_port
  {
    string const name;

    std::fstream fstream;

    template <typename S, typename... Ts>
    explicit file_port(S const& name, Ts&&... xs)
      : name { name }
      , fstream { name, std::forward<decltype(xs)>(xs)... }
    {}

    auto close() -> void override;

    auto is_open() const -> bool override;

    operator std::istream &() override;

    operator std::istream const&() const override;

    operator std::ostream &() override;
  };

  auto operator <<(std::ostream &, file_port const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_FILE_PORT_HPP
