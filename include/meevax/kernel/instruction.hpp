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

#include <meevax/kernel/pair.hpp>

namespace meevax
{
inline namespace kernel
{
  enum class mnemonic : std::uint8_t
  {
    CALL,               // a.k.a AP
    CONS,
    DEFINE,
    DROP,
    DUMMY,              // a.k.a DUM
    FORK,
    JOIN,
    LOAD_CLOSURE,       // a.k.a LDF
    LOAD_CONSTANT,      // a.k.a LDC
    LOAD_CONTINUATION,
    LOAD_GLOBAL,
    LOAD_LOCAL,
    LOAD_VARIADIC,
    RECURSIVE_CALL,     // a.k.a RAP
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

  auto disassemble(std::ostream &, pair::const_reference, std::size_t = 1) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_INSTRUCTION_HPP
