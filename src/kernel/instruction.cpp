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
      case mnemonic::CALL:              return "CALL";
      case mnemonic::CONS:              return "CONS";
      case mnemonic::DEFINE:            return "DEFINE";
      case mnemonic::DROP:              return "DROP";
      case mnemonic::DUMMY:             return "DUMMY";
      case mnemonic::FORK:              return "FORK";
      case mnemonic::JOIN:              return "JOIN";
      case mnemonic::LET_SYNTAX:        return "LET_SYNTAX";
      case mnemonic::LOAD_ABSOLUTE:     return "LOAD_ABSOLUTE";
      case mnemonic::LOAD_CLOSURE:      return "LOAD_CLOSURE";
      case mnemonic::LOAD_CONSTANT:     return "LOAD_CONSTANT";
      case mnemonic::LOAD_CONTINUATION: return "LOAD_CONTINUATION";
      case mnemonic::LOAD_RELATIVE:     return "LOAD_RELATIVE";
      case mnemonic::LOAD_VARIADIC:     return "LOAD_VARIADIC";
      case mnemonic::RECURSIVE_CALL:    return "RECURSIVE_CALL";
      case mnemonic::RETURN:            return "RETURN";
      case mnemonic::SELECT:            return "SELECT";
      case mnemonic::STOP:              return "STOP";
      case mnemonic::STORE_ABSOLUTE:    return "STORE_ABSOLUTE";
      case mnemonic::STORE_RELATIVE:    return "STORE_RELATIVE";
      case mnemonic::STORE_VARIADIC:    return "STORE_VARIADIC";
      case mnemonic::TAIL_CALL:         return "TAIL_CALL";
      case mnemonic::TAIL_SELECT:       return "TAIL_SELECT";

      default:
        throw std::logic_error(__func__);
    }
  }

  auto operator <<(std::ostream & os, instruction const& datum) -> std::ostream &
  {
    return os << underline << static_cast<std::string>(datum) << reset;
  }

  auto disassemble(std::ostream & os, const_reference c, std::size_t depth) -> std::ostream &
  {
    assert(0 < depth);

    static std::size_t index = 0;

    index = (depth == 1 ? 0 : index + 1);

    for (auto iter = std::cbegin(c); iter != std::cend(c); ++iter)
    {
      os << faint << "; " << std::setw(4) << std::right << std::to_string(index) << "  " << reset;

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
      case mnemonic::CALL:
      case mnemonic::CONS:
      case mnemonic::DROP:
      case mnemonic::DUMMY:
      case mnemonic::JOIN:
      case mnemonic::RECURSIVE_CALL:
      case mnemonic::TAIL_CALL:
        os << *iter << "\n";
        ++index;
        break;

      case mnemonic::RETURN:
      case mnemonic::STOP:
        os << *iter << magenta << "\t)\n" << reset;
        ++index;
        break;

      case mnemonic::FORK:
      case mnemonic::LET_SYNTAX:
      case mnemonic::LOAD_CONSTANT:
      case mnemonic::LOAD_RELATIVE:
      case mnemonic::LOAD_VARIADIC:
      case mnemonic::STORE_RELATIVE:
      case mnemonic::STORE_VARIADIC:
        os << *iter << " " << *++iter << "\n";
        index += 2;
        break;

      case mnemonic::DEFINE:
      case mnemonic::LOAD_ABSOLUTE:
      case mnemonic::STORE_ABSOLUTE:
        os << *iter << " " << car(*++iter) << "\n";
        index += 2;
        break;

      case mnemonic::LOAD_CLOSURE:
      case mnemonic::LOAD_CONTINUATION:
        os << *iter << "\n";
        disassemble(os, *++iter, depth + 1);
        ++index;
        break;

      case mnemonic::SELECT:
      case mnemonic::TAIL_SELECT:
        os << *iter << "\n";
        disassemble(os, *++iter, depth + 1);
        disassemble(os, *++iter, depth + 1);
        ++index;
        break;

      default:
        assert(false);
      }
    }

    return os;
  }

  static_assert(std::is_pod<instruction>::value);
  static_assert(std::is_standard_layout<instruction>::value);
  static_assert(std::is_trivial<instruction>::value);
} // namespace kernel
} // namespace meevax
