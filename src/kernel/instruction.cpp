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
      case instruction::call:              return os << "call";
      case instruction::current:           return os << "current";
      case instruction::drop:              return os << "drop";
      case instruction::install:           return os << "install";
      case instruction::list_values:       return os << "list-values";
      case instruction::load_absolute:     return os << "load-absolute";
      case instruction::load_closure:      return os << "load-closure";
      case instruction::load_constant:     return os << "load-constant";
      case instruction::load_relative:     return os << "load-relative";
      case instruction::load_variadic:     return os << "load-variadic";
      case instruction::select:            return os << "select";
      case instruction::store_absolute:    return os << "store-absolute";
      case instruction::store_relative:    return os << "store-relative";
      case instruction::store_variadic:    return os << "store-variadic";

      default:
        assert(false);
        return os;
    }
  }
} // namespace meevax::kernel
