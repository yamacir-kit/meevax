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
    CALL,               // a.k.a APP
    CONS,
    DEFINE,
    DROP,
    DUMMY,              // a.k.a DUM
    FORK,
    JOIN,
    LET_SYNTAX,
    LOAD_ABSOLUTE,      // a.k.a LDG
    LOAD_CLOSURE,       // a.k.a LDF
    LOAD_CONSTANT,      // a.k.a LDC
    LOAD_CONTINUATION,
    LOAD_RELATIVE,      // a.k.a LDL
    LOAD_VARIADIC,
    RECURSIVE_CALL,     // a.k.a RAP
    RETURN,
    SELECT,
    STOP,
    STORE_ABSOLUTE,
    STORE_RELATIVE,
    STORE_VARIADIC,
    TAIL_CALL,
    TAIL_SELECT,

    size,
  };

  struct instruction
  {
    const mnemonic value;

    explicit operator std::string() const;
  };

  namespace code
  {
    let extern const call;
    let extern const cons;
    let extern const define;
    let extern const drop;
    let extern const dummy;
    let extern const fork;
    let extern const join;
    let extern const let_syntax;
    let extern const load_absolute;
    let extern const load_closure;
    let extern const load_constant;
    let extern const load_continuation;
    let extern const load_relative;
    let extern const load_variadic;
    let extern const recursive_call;
    let extern const return_;
    let extern const select;
    let extern const stop;
    let extern const store_absolute;
    let extern const store_relative;
    let extern const store_variadic;
    let extern const tail_call;
    let extern const tail_select;
  }

  auto operator <<(std::ostream &, instruction const&) -> std::ostream &;

  auto disassemble(std::ostream &, const_reference, std::size_t = 1) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_INSTRUCTION_HPP
