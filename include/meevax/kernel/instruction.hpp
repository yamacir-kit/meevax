#ifndef INCLUDED_MEEVAX_KERNEL_INSTRUCTION_HPP
#define INCLUDED_MEEVAX_KERNEL_INSTRUCTION_HPP

#include <boost/preprocessor.hpp>

#include <meevax/kernel/object.hpp>

namespace meevax::kernel
{
  #define MNEMONICS                                                            \
    (APPLY)                                                                    \
    (APPLY_TAIL)                                                               \
    (DEFINE)                                                                   \
    (FORK)                                                                     \
    (JOIN)                                                                     \
    (LOAD_CLOSURE)                                                             \
    (LOAD_CONSTANT)                                                            \
    (LOAD_CONTINUATION)                                                        \
    (LOAD_GLOBAL)                                                              \
    (LOAD_LOCAL)                                                               \
    (LOAD_SYNTACTIC_CONTINUATION)                                              \
    (LOAD_VARIADIC)                                                            \
    (POP)                                                                      \
    (PUSH)                                                                     \
    (RETURN)                                                                   \
    (SELECT)                                                                   \
    (SELECT_TAIL)                                                              \
    (STOP)                                                                     \
    (STORE_GLOBAL)                                                             \
    (STORE_LOCAL)                                                              \
    (STORE_VARIADIC)

  enum class mnemonic
    : std::int8_t
  {
    BOOST_PP_SEQ_ENUM(MNEMONICS)
  };

  struct instruction
  {
    using identity = instruction;

    const mnemonic code;

    template <typename... Ts>
    explicit instruction(Ts&&... operands)
      : code {std::forward<decltype(operands)>(operands)...}
    {}

    int value() const noexcept
    {
      return
        static_cast<
          typename std::underlying_type<mnemonic>::type
        >(code);
    }

    friend auto operator<<(std::ostream& os, const identity& i)
      -> decltype(auto)
    {
      os << highlight::system;

      switch (i.code)
      {
      #define MNEMONIC_CASE(_, AUX, EACH)                                      \
      case mnemonic::EACH:                                                     \
        os << BOOST_PP_STRINGIZE(EACH);                                        \
        break;

        BOOST_PP_SEQ_FOR_EACH(MNEMONIC_CASE, _, MNEMONICS)
      }

      // os << "#" << std::hex << i.value();

      return os << attribute::normal;
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_INSTRUCTION_HPP

