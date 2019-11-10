#ifndef INCLUDED_MEEVAX_KERNEL_INSTRUCTION_HPP
#define INCLUDED_MEEVAX_KERNEL_INSTRUCTION_HPP

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

  enum class code
  {
    BOOST_PP_SEQ_ENUM(INSTRUCTIONS)
  };

  struct instruction
  {
    const code value;

    template <typename... Ts>
    instruction(Ts&&... operands)
      : value {std::forward<decltype(operands)>(operands)...}
    {}
  };

  std::ostream& operator<<(std::ostream& os, const instruction& instruction)
  {
    os << highlight::kernel;

    switch (instruction.value)
    {
    #define INSTRUCTION_CASE(_, AUX, EACH) \
    case code::EACH: \
      os << BOOST_PP_STRINGIZE(EACH); \
      break;

      BOOST_PP_SEQ_FOR_EACH(INSTRUCTION_CASE, _, INSTRUCTIONS)

    #undef INSTRUCTION_CASE
    }

    return os << attribute::normal;
  }

  static const auto _apply_               {make<instruction>(code::APPLY)};
  static const auto _apply_tail_          {make<instruction>(code::APPLY_TAIL)};
  static const auto _define_              {make<instruction>(code::DEFINE)};
  static const auto _join_                {make<instruction>(code::JOIN)};
  static const auto _load_global_         {make<instruction>(code::LOAD_GLOBAL)};
  static const auto _load_literal_        {make<instruction>(code::LOAD_LITERAL)};
  static const auto _load_local_          {make<instruction>(code::LOAD_LOCAL)};
  static const auto _load_local_variadic_ {make<instruction>(code::LOAD_LOCAL_VARIADIC)};
  static const auto _make_closure_        {make<instruction>(code::MAKE_CLOSURE)};
  static const auto _make_continuation_   {make<instruction>(code::MAKE_CONTINUATION)};
  static const auto _make_environment_    {make<instruction>(code::MAKE_ENVIRONMENT)};
  static const auto _pop_                 {make<instruction>(code::POP)};
  static const auto _push_                {make<instruction>(code::PUSH)};
  static const auto _return_              {make<instruction>(code::RETURN)};
  static const auto _select_              {make<instruction>(code::SELECT)};
  static const auto _select_tail_         {make<instruction>(code::SELECT_TAIL)};
  static const auto _set_global_          {make<instruction>(code::SET_GLOBAL)};
  static const auto _set_local_           {make<instruction>(code::SET_LOCAL)};
  static const auto _set_local_variadic_  {make<instruction>(code::SET_LOCAL_VARIADIC)};
  static const auto _stop_                {make<instruction>(code::STOP)};

  static const auto _make_syntactic_continuation_ {
    make<instruction>(code::MAKE_SYNTACTIC_CONTINUATION)
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_INSTRUCTION_HPP

