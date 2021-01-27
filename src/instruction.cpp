#include <meevax/kernel/instruction.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/posix/vt102.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator <<(std::ostream & os, instruction const& datum) -> std::ostream &
  {
    os << underline;

    switch (datum.code)
    {
    #define MNEMONIC_CASE(_, AUX, EACH)                                        \
    case mnemonic::EACH:                                                       \
      os << BOOST_PP_STRINGIZE(EACH);                                          \
      break;

      BOOST_PP_SEQ_FOR_EACH(MNEMONIC_CASE, _, MNEMONICS)
    }

    // os << "#" << std::hex << std::setw(2) << std::setfill('0') << i.value();

    return os << reset;
  }

  auto disassemble(std::ostream & os, let const& c, std::size_t depth) -> std::ostream &
  {
    assert(0 < depth);

    for (auto iter = std::cbegin(c); iter != std::cend(c); ++iter)
    {
      os << "; ";

      if (iter == c)
      {
        os << bytestring(4 * (depth - 1), ' ') << magenta << "(   " << reset;
      }
      else
      {
        os << bytestring(4 * depth, ' ');
      }

      switch ((*iter).as<instruction>().code)
      {
      case mnemonic::CALL:
      case mnemonic::CONS:
      case mnemonic::DROP:
      case mnemonic::JOIN:
      case mnemonic::TAIL_CALL:
        os << *iter << "\n";
        break;

      case mnemonic::RETURN:
      case mnemonic::STOP:
        os << *iter << magenta << "\t)\n" << reset;
        break;

      case mnemonic::FORK:
      case mnemonic::LOAD_CONSTANT:
      case mnemonic::LOAD_LOCAL:
      case mnemonic::LOAD_VARIADIC:
      case mnemonic::STORE_GLOBAL:
      case mnemonic::STORE_LOCAL:
      case mnemonic::STORE_VARIADIC:
      case mnemonic::STRIP:
        os << *iter << " " << *++iter << "\n";
        break;

      case mnemonic::DEFINE:
      case mnemonic::LOAD_GLOBAL:
        os << *iter << " " << car(*++iter) << "\n";
        break;

      case mnemonic::LOAD_CLOSURE:
      case mnemonic::LOAD_CONTINUATION:
        os << *iter << "\n";
        disassemble(os, *++iter, depth + 1);
        break;

      case mnemonic::SELECT:
      case mnemonic::TAIL_SELECT:
        os << *iter << "\n";
        disassemble(os, *++iter, depth + 1);
        disassemble(os, *++iter, depth + 1);
        break;

      default:
        assert(false);
      }
    }

    return os;
  }
} // namespace kernel
} // namespace meevax
