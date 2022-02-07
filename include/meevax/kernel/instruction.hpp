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

#ifndef INCLUDED_MEEVAX_KERNEL_INSTRUCTION_HPP
#define INCLUDED_MEEVAX_KERNEL_INSTRUCTION_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax
{
inline namespace kernel
{
  enum class mnemonic : std::uint8_t
  {
    call,              // a.k.a APP
    cons,              //
    define,            //
    drop,              //
    dummy,             // a.k.a DUM
    fork,              //
    join,              //
    let_syntax,        //
    letrec,            // a.k.a RAP
    letrec_syntax,     //
    load_absolute,     // a.k.a LDG
    load_closure,      // a.k.a LDF
    load_constant,     // a.k.a LDC
    load_continuation, //
    load_relative,     // a.k.a LDL
    load_variadic,     //
    return_,           // a.k.a RTN
    select,            // a.k.a SEL
    stop,              //
    store_absolute,    //
    store_relative,    //
    store_variadic,    //
    tail_call,         //
    tail_select,       //
  };

  struct instruction
  {
    const mnemonic value;

    // TODO explicit instruction(std::string const&);

    explicit operator std::string() const;
  };

  auto operator <<(std::ostream &, instruction const&) -> std::ostream &;

  auto disassemble(std::ostream &, const_reference, std::size_t = 1) -> void;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_INSTRUCTION_HPP
