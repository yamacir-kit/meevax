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

#ifndef INCLUDED_MEEVAX_KERNEL_INSTRUCTION_HPP
#define INCLUDED_MEEVAX_KERNEL_INSTRUCTION_HPP

#include <ostream>

namespace meevax::inline kernel
{
  enum class instruction
  {
    secd_current,           //
    secd_drop,              //
    secd_install,           //
    secd_list_values,       //
    secd_load_absolute,     // a.k.a LDG
    secd_load_closure,      // a.k.a LDF
    secd_load_constant,     // a.k.a LDC
    secd_load_relative,     // a.k.a LDL
    secd_load_variadic,     //
    secd_select,            // a.k.a SEL
    secd_store_absolute,    //
    secd_store_relative,    //
    secd_store_variadic,    //
    secd_tail_call,         //
  };

  auto operator <<(std::ostream &, instruction const&) -> std::ostream &;
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_INSTRUCTION_HPP
