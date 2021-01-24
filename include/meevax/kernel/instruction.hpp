#ifndef INCLUDED_MEEVAX_KERNEL_INSTRUCTION_HPP
#define INCLUDED_MEEVAX_KERNEL_INSTRUCTION_HPP

#include <boost/preprocessor.hpp>

#include <meevax/kernel/object.hpp>

// TODO
//   convert to lower-case
//   use #! as external representation

namespace meevax
{
inline namespace kernel
{
  #define MNEMONICS                                                            \
    (CALL)                                                                     \
    (CONS)                                                                     \
    (DEFINE)                                                                   \
    (DROP)                                                                     \
    (FORK)                                                                     \
    (JOIN)                                                                     \
    (LOAD_CLOSURE)                                                             \
    (LOAD_CONSTANT)                                                            \
    (LOAD_CONTINUATION)                                                        \
    (LOAD_GLOBAL)                                                              \
    (LOAD_LOCAL)                                                               \
    (LOAD_VARIADIC)                                                            \
    (RETURN)                                                                   \
    (SELECT)                                                                   \
    (STOP)                                                                     \
    (STORE_GLOBAL)                                                             \
    (STORE_LOCAL)                                                              \
    (STORE_VARIADIC)                                                           \
    (STRIP)                                                                    \
    (TAIL_CALL)                                                                \
    (TAIL_SELECT)                                                              \

  enum class mnemonic
    : std::int8_t
  {
    BOOST_PP_SEQ_ENUM(MNEMONICS)
  };

  struct instruction
  {
    const mnemonic code;

    template <typename... Ts>
    explicit instruction(Ts&&... xs)
      : code { std::forward<decltype(xs)>(xs)... }
    {}

    using value_type = typename std::underlying_type<mnemonic>::type;

    constexpr auto value() const noexcept -> value_type
    {
      return static_cast<value_type>(code);
    }
  };

  auto operator <<(std::ostream &, instruction const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_INSTRUCTION_HPP
