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

#include <cassert>

#include <meevax/kernel/instruction.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator <<(std::ostream & os, instruction const& datum) -> std::ostream &
  {
    switch (datum)
    {
      case instruction::call:              return os << "call";
      case instruction::cons:              return os << "cons";
      case instruction::current:           return os << "current";
      case instruction::define_syntax:     return os << "define-syntax";
      case instruction::drop:              return os << "drop";
      case instruction::dummy:             return os << "dummy";
      case instruction::install:           return os << "install";
      case instruction::join:              return os << "join";
      case instruction::let_syntax:        return os << "let-syntax";
      case instruction::letrec:            return os << "letrec";
      case instruction::letrec_syntax:     return os << "letrec-syntax";
      case instruction::load_absolute:     return os << "load-absolute";
      case instruction::load_closure:      return os << "load-closure";
      case instruction::load_constant:     return os << "load-constant";
      case instruction::load_continuation: return os << "load-continuation";
      case instruction::load_relative:     return os << "load-relative";
      case instruction::load_variadic:     return os << "load-variadic";
      case instruction::return_:           return os << "return";
      case instruction::select:            return os << "select";
      case instruction::stop:              return os << "stop";
      case instruction::store_absolute:    return os << "store-absolute";
      case instruction::store_relative:    return os << "store-relative";
      case instruction::store_variadic:    return os << "store-variadic";
      case instruction::tail_call:         return os << "tail-call";
      case instruction::tail_letrec:       return os << "tail-letrec";
      case instruction::tail_select:       return os << "tail-select";

      default:
        assert(false);
        return os;
    }
  }

  auto instruction_length(instruction const& datum) -> std::size_t
  {
    switch (datum)
    {
      case instruction::call:
      case instruction::cons:
      case instruction::drop:
      case instruction::dummy:
      case instruction::join:
      case instruction::letrec:
      case instruction::return_:
      case instruction::stop:
      case instruction::tail_call:
      case instruction::tail_letrec:
        return 1;

      case instruction::current:
      case instruction::define_syntax:
      case instruction::install:
      case instruction::let_syntax:
      case instruction::letrec_syntax:
      case instruction::load_absolute:
      case instruction::load_closure:
      case instruction::load_constant:
      case instruction::load_continuation:
      case instruction::load_relative:
      case instruction::load_variadic:
      case instruction::store_absolute:
      case instruction::store_relative:
      case instruction::store_variadic:
        return 2;

      case instruction::select:
      case instruction::tail_select:
        return 3;

      default:
        assert(false);
        return 0;
    }
  }
} // namespace kernel
} // namespace meevax
