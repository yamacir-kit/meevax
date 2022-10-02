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

#include <meevax/iostream/lexical_cast.hpp>
#include <meevax/kernel/mnemonic.hpp>
#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  auto disassemble(std::ostream & os, const_reference c, std::size_t depth) -> void
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

      switch ((*iter).as<mnemonic>())
      {
      case mnemonic::call:
      case mnemonic::cons:
      case mnemonic::drop:
      case mnemonic::dummy:
      case mnemonic::join:
      case mnemonic::letrec:
      case mnemonic::load_r0:
      case mnemonic::return_:
      case mnemonic::stop:
      case mnemonic::store_r0:
      case mnemonic::tail_call:
        os << *iter << "\n";
        ++offset;
        break;

      case mnemonic::define:
      case mnemonic::define_syntax:
      case mnemonic::let_syntax:
      case mnemonic::letrec_syntax:
      case mnemonic::load_absolute:
      case mnemonic::load_constant:
      case mnemonic::load_relative:
      case mnemonic::load_variadic:
      case mnemonic::store_absolute:
      case mnemonic::store_relative:
      case mnemonic::store_variadic:
        os << *iter << " " << *++iter << "\n";
        offset += 2;
        break;

      case mnemonic::load_closure:
      case mnemonic::load_continuation:
        os << *iter << "\n";
        ++offset;
        disassemble(os, *++iter, depth + 1);
        break;

      case mnemonic::select:
      case mnemonic::tail_select:
        os << *iter << "\n";
        ++offset;
        disassemble(os, *++iter, depth + 1);
        disassemble(os, *++iter, depth + 1);
        break;
      }
    }
  }
} // namespace kernel
} // namespace meevax
