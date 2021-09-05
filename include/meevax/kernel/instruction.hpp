/*
   Copyright 2018-2021 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_KERNEL_INSTRUCTION_HPP
#define INCLUDED_MEEVAX_KERNEL_INSTRUCTION_HPP

#include <meevax/kernel/object.hpp>
#include <meevax/type_traits/underlying_cast.hpp>

// TODO
//   convert to lower-case
//   use #! as external representation

namespace meevax
{
inline namespace kernel
{
  enum class mnemonic : std::uint8_t
  {
    CALL,
    CONS,
    DEFINE,
    DROP,
    FORK,
    JOIN,
    LOAD_CLOSURE,
    LOAD_CONSTANT,
    LOAD_CONTINUATION,
    LOAD_GLOBAL,
    LOAD_LOCAL,
    LOAD_VARIADIC,
    RETURN,
    SELECT,
    STOP,
    STORE_GLOBAL,
    STORE_LOCAL,
    STORE_VARIADIC,
    TAIL_CALL,
    TAIL_SELECT,
  };

  struct instruction
  {
    using value_type = mnemonic;

    const value_type value;

    explicit operator std::string() const;
  };

  auto operator <<(std::ostream &, instruction const&) -> std::ostream &;

  auto disassemble(std::ostream &, let const&, std::size_t = 1) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_INSTRUCTION_HPP
