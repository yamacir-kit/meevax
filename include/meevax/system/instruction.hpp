#ifndef INCLUDED_MEEVAX_SYSTEM_INSTRUCTION_HPP
#define INCLUDED_MEEVAX_SYSTEM_INSTRUCTION_HPP

#include <meevax/system/pair.hpp>

namespace meevax::system
{
  enum class code // TODO RENAME TO "code"
  {
    APPLY, // XXX 紛らわしいから CALL と TAIL_CALL に変える？
    APPLY_TAIL,
    DEFINE,
    JOIN,
    LOAD_GLOBAL,
    LOAD_LITERAL,
    LOAD_LOCAL,
    LOAD_LOCAL_VARIADIC,
    MAKE_CLOSURE,
    MAKE_CONTINUATION,
    MAKE_MODULE,
    POP,
    PUSH,
    RETURN,
    SELECT,
    SELECT_TAIL,
    SET_GLOBAL,
    SET_LOCAL,
    SET_LOCAL_VARIADIC,
    STOP,
  };

  struct instruction // XXX CONVERT TO NUMBER TYPE?
  {
    const code value;

    template <typename... Ts>
    instruction(Ts&&... args)
      : value {std::forward<Ts>(args)...}
    {}
  };

  std::ostream& operator<<(std::ostream& os, const instruction& instruction)
  {
    os << "\x1b[32m";

    switch (instruction.value)
    {
    case code::APPLY:
      os << "apply";
      break;

    case code::APPLY_TAIL:
      os << "apply-tail";
      break;

    case code::DEFINE:
      os << "define";
      break;

    case code::JOIN:
      os << "join";
      break;

    case code::LOAD_GLOBAL:
      os << "load-global";
      break;

    case code::LOAD_LITERAL:
      os << "load-literal";
      break;

    case code::LOAD_LOCAL:
      os << "load-local";
      break;

    case code::LOAD_LOCAL_VARIADIC:
      os << "load-local-variadic";
      break;

    case code::MAKE_CLOSURE:
      os << "make-closure";
      break;

    case code::MAKE_CONTINUATION:
      os << "make-continuation";
      break;

    case code::MAKE_MODULE:
      os << "make-module";
      break;

    case code::POP:
      os << "pop";
      break;

    case code::PUSH:
      os << "push";
      break;

    case code::RETURN:
      os << "return";
      break;

    case code::SELECT:
      os << "select";
      break;

    case code::SELECT_TAIL:
      os << "select-tail";
      break;

    case code::SET_GLOBAL:
      os << "set-global";
      break;

    case code::SET_LOCAL:
      os << "set-local";
      break;

    case code::SET_LOCAL_VARIADIC:
      os << "set-local-variadic";
      break;

    case code::STOP:
      os << "stop";
      break;
    }

    return os << "\x1b[0m";
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
  static const auto _make_module_         {make<instruction>(code::MAKE_MODULE)};
  static const auto _pop_                 {make<instruction>(code::POP)};
  static const auto _push_                {make<instruction>(code::PUSH)};
  static const auto _return_              {make<instruction>(code::RETURN)};
  static const auto _select_              {make<instruction>(code::SELECT)};
  static const auto _select_tail_         {make<instruction>(code::SELECT_TAIL)};
  static const auto _set_global_          {make<instruction>(code::SET_GLOBAL)};
  static const auto _set_local_           {make<instruction>(code::SET_LOCAL)};
  static const auto _set_local_variadic_  {make<instruction>(code::SET_LOCAL_VARIADIC)};
  static const auto _stop_                {make<instruction>(code::STOP)};
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_INSTRUCTION_HPP

