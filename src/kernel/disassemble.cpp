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

#include <meevax/kernel/disassemble.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/kernel/mnemonic.hpp>

namespace meevax
{
inline namespace kernel
{
  auto disassemble(std::ostream & os, object const& c, std::size_t depth) -> void
  {
    depth = std::clamp(depth, std::numeric_limits<std::size_t>::min(),
                              std::numeric_limits<std::size_t>::max());

    static std::size_t offset = 0;

    offset = (depth == 1 ? 0 : offset + 1);

    for (auto iter = std::cbegin(c); iter != std::cend(c); ++iter)
    {
      os << faint("; ", std::setw(4), std::right, std::to_string(offset), "  ");

      if (iter == c)
      {
        os << std::string(4 * (depth - 1), ' ') << magenta("(   ");
      }
      else
      {
        os << std::string(4 * depth, ' ');
      }

      switch ((*iter).as<instruction>())
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
        os << *iter << "\n";
        offset += 1;
        break;

      case instruction::define:
      case instruction::define_syntax:
      case instruction::let_syntax:
      case instruction::letrec_syntax:
      case instruction::load_absolute:
      case instruction::load_auxiliary:
      case instruction::load_constant:
      case instruction::load_relative:
      case instruction::load_variadic:
      case instruction::store_absolute:
      case instruction::store_auxiliary:
      case instruction::store_relative:
      case instruction::store_variadic:
        os << *iter << " " << *++iter << "\n";
        offset += 2;
        break;

      case instruction::load_closure:
      case instruction::load_continuation:
        os << *iter << "\n";
        offset += 1;
        disassemble(os, *++iter, depth + 1);
        break;

      case instruction::select:
      case instruction::tail_select:
        os << *iter << "\n";
        offset += 1;
        disassemble(os, *++iter, depth + 1);
        disassemble(os, *++iter, depth + 1);
        break;
      }
    }
  }
} // namespace kernel
} // namespace meevax
