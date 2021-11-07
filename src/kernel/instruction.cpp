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

#include <cstddef>
#include <meevax/kernel/instruction.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  instruction::operator std::string() const
  {
    switch (value)
    {
      case mnemonic::call:              return "call";
      case mnemonic::cons:              return "cons";
      case mnemonic::define:            return "define";
      case mnemonic::drop:              return "drop";
      case mnemonic::expand:            return "expand";
      case mnemonic::extend:            return "extend";
      case mnemonic::fork:              return "fork";
      case mnemonic::join:              return "join";
      case mnemonic::load_absolute:     return "load-absolute";
      case mnemonic::load_closure:      return "load-closure";
      case mnemonic::load_constant:     return "load-constant";
      case mnemonic::load_continuation: return "load-continuation";
      case mnemonic::load_relative:     return "load-relative";
      case mnemonic::load_variadic:     return "load-variadic";
      case mnemonic::recursive_call:    return "recursive-call";
      case mnemonic::recursive_expand:  return "recursive-expand";
      case mnemonic::return_:           return "return";
      case mnemonic::select:            return "select";
      case mnemonic::stop:              return "stop";
      case mnemonic::store_absolute:    return "store-absolute";
      case mnemonic::store_relative:    return "store-relative";
      case mnemonic::store_variadic:    return "store-variadic";
      case mnemonic::tail_call:         return "tail-call";
      case mnemonic::tail_select:       return "tail-select";

      default:
        throw std::logic_error(__func__);
    }
  }

  auto operator <<(std::ostream & os, instruction const& datum) -> std::ostream &
  {
    return os << '%' << static_cast<std::string>(datum);
  }

  auto disassemble(std::ostream & os, const_reference c, std::size_t depth) -> void
  {
    depth = std::clamp(depth, std::numeric_limits<std::size_t>::min(),
                              std::numeric_limits<std::size_t>::max());

    static std::size_t offset = 0;

    offset = (depth == 1 ? 0 : offset + 1);

    for (auto iter = std::cbegin(c); iter != std::cend(c); ++iter)
    {
      os << faint << "; " << std::setw(4) << std::right << std::to_string(offset) << "  " << reset;

      if (iter == c)
      {
        os << std::string(4 * (depth - 1), ' ') << magenta << "(   " << reset;
      }
      else
      {
        os << std::string(4 * depth, ' ');
      }

      switch ((*iter).as<instruction>().value)
      {
      case mnemonic::call:
      case mnemonic::cons:
      case mnemonic::drop:
      case mnemonic::extend:
      case mnemonic::join:
      case mnemonic::recursive_call:
      case mnemonic::tail_call:
        os << *iter << "\n";
        ++offset;
        break;

      case mnemonic::return_:
      case mnemonic::stop:
        os << *iter << magenta << ")\n" << reset;
        ++offset;
        break;

      case mnemonic::define:
      case mnemonic::expand:
      case mnemonic::fork:
      case mnemonic::load_absolute:
      case mnemonic::load_constant:
      case mnemonic::load_relative:
      case mnemonic::load_variadic:
      case mnemonic::recursive_expand:
      case mnemonic::store_absolute:
      case mnemonic::store_relative:
      case mnemonic::store_variadic:
        os << *iter << " " << *++iter << "\n";
        offset += 2;
        break;

      case mnemonic::load_closure:
      case mnemonic::load_continuation:
        os << *iter << "\n";
        disassemble(os, *++iter, depth + 1);
        ++offset;
        break;

      case mnemonic::select:
      case mnemonic::tail_select:
        os << *iter << "\n";
        disassemble(os, *++iter, depth + 1);
        disassemble(os, *++iter, depth + 1);
        ++offset;
        break;

      default:
        assert(false);
      }
    }
  }

  static_assert(std::is_pod<instruction>::value);
  static_assert(std::is_standard_layout<instruction>::value);
  static_assert(std::is_trivial<instruction>::value);
} // namespace kernel
} // namespace meevax
