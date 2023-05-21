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

#ifndef INCLUDED_MEEVAX_KERNEL_TEXTUAL_INPUT_PORT_HPP
#define INCLUDED_MEEVAX_KERNEL_TEXTUAL_INPUT_PORT_HPP

#include <istream>

#include <meevax/kernel/character.hpp>
#include <meevax/kernel/input_port.hpp>
#include <meevax/kernel/pair.hpp>
#include <meevax/kernel/textual_port.hpp>

namespace meevax
{
inline namespace kernel
{
  struct textual_input_port : public virtual textual_port, public virtual input_port
  {
    auto get() -> object;

    auto get(std::size_t) -> object;

    auto get_codepoint() -> character::int_type;

    auto get_line() -> object;

    auto get_ready() const -> bool;

    auto peek() -> object;

    auto read() -> object;

    explicit virtual operator std::istream &() = 0;

    explicit virtual operator std::istream const&() const = 0;
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_TEXTUAL_INPUT_PORT_HPP
