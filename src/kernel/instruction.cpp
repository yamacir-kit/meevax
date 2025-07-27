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

#include <cassert>

#include <meevax/kernel/instruction.hpp>

namespace meevax::inline kernel
{
  auto operator <<(std::ostream & os, instruction const& datum) -> std::ostream &
  {
    switch (datum)
    {
      case instruction::secd_call:              return os << "call";
      case instruction::secd_cons:              return os << "cons";
      case instruction::secd_current:           return os << "current";
      case instruction::secd_drop:              return os << "drop";
      case instruction::secd_dummy:             return os << "dummy";
      case instruction::secd_install:           return os << "install";
      case instruction::secd_join:              return os << "join";
      case instruction::secd_letrec:            return os << "letrec";
      case instruction::secd_load_absolute:     return os << "load-absolute";
      case instruction::secd_load_closure:      return os << "load-closure";
      case instruction::secd_load_constant:     return os << "load-constant";
      case instruction::secd_load_continuation: return os << "load-continuation";
      case instruction::secd_load_relative:     return os << "load-relative";
      case instruction::secd_load_variadic:     return os << "load-variadic";
      case instruction::secd_return:            return os << "return";
      case instruction::secd_select:            return os << "select";
      case instruction::secd_stop:              return os << "stop";
      case instruction::secd_store_absolute:    return os << "store-absolute";
      case instruction::secd_store_relative:    return os << "store-relative";
      case instruction::secd_store_variadic:    return os << "store-variadic";
      case instruction::secd_tail_call:         return os << "tail-call";
      case instruction::secd_tail_letrec:       return os << "tail-letrec";
      case instruction::secd_tail_select:       return os << "tail-select";

      default:
        assert(false);
        return os;
    }
  }
} // namespace meevax::kernel
