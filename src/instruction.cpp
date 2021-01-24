#include <meevax/kernel/instruction.hpp>
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
} // namespace kernel
} // namespace meevax
