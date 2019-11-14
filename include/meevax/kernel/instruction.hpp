#ifndef INCLUDED_MEEVAX_KERNEL_INSTRUCTION_HPP
#define INCLUDED_MEEVAX_KERNEL_INSTRUCTION_HPP

#include <cstdint>

#include <boost/preprocessor.hpp>

#include <meevax/kernel/object.hpp>

namespace meevax::kernel
{
  #define INSTRUCTIONS \
    (APPLY) \
    (APPLY_TAIL) \
    (DEFINE) \
    (JOIN) \
    (LOAD_GLOBAL) \
    (LOAD_LITERAL) \
    (LOAD_LOCAL) \
    (LOAD_LOCAL_VARIADIC) \
    (MAKE_CLOSURE) \
    (MAKE_CONTINUATION) \
    (MAKE_ENVIRONMENT) \
    (MAKE_SYNTACTIC_CONTINUATION) \
    (POP) \
    (PUSH) \
    (RETURN) \
    (SELECT) \
    (SELECT_TAIL) \
    (SET_GLOBAL) \
    (SET_LOCAL) \
    (SET_LOCAL_VARIADIC) \
    (STOP)

  enum class mnemonic
    : std::int8_t
  {
    BOOST_PP_SEQ_ENUM(INSTRUCTIONS)
  };

  struct instruction
  {
    const mnemonic code;

    template <typename... Ts>
    explicit instruction(Ts&&... operands)
      : code {std::forward<decltype(operands)>(operands)...}
    {}
  };

  std::ostream& operator<<(std::ostream& os, const instruction& instruction)
  {
    os << highlight::kernel;

    switch (instruction.code)
    {
    #define INSTRUCTION_CASE(_, AUX, EACH) \
    case mnemonic::EACH: \
      os << BOOST_PP_STRINGIZE(EACH); \
      break;

      BOOST_PP_SEQ_FOR_EACH(INSTRUCTION_CASE, _, INSTRUCTIONS)

    #undef INSTRUCTION_CASE
    }

    return os << attribute::normal;
  }
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_INSTRUCTION_HPP

