#ifndef INCLUDED_MEEVAX_SYSTEM_INSTRUCTION_HPP
#define INCLUDED_MEEVAX_SYSTEM_INSTRUCTION_HPP

#include <meevax/system/pair.hpp>

namespace meevax::system
{
  enum class secd // TODO RENAME TO "code"
  {
    APPLY,
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
    // SELECT_TAIL,
    SET_GLOBAL,
    SET_LOCAL,
    SET_LOCAL_VARIADIC,
    STOP,
  };

  struct instruction // XXX CONVERT TO NUMBER TYPE?
  {
    const secd code;

    template <typename... Ts>
    instruction(Ts&&... args)
      : code {std::forward<Ts>(args)...}
    {}
  };

  std::ostream& operator<<(std::ostream& os, const instruction& instruction)
  {
    os << "\x1b[32m";

    switch (instruction.code)
    {
    case secd::APPLY:
      os << "apply";
      break;

    case secd::DEFINE:
      os << "define";
      break;

    case secd::JOIN:
      os << "join";
      break;

    case secd::LOAD_GLOBAL:
      os << "load-global";
      break;

    case secd::LOAD_LITERAL:
      os << "load-literal";
      break;

    case secd::LOAD_LOCAL:
      os << "load-local";
      break;

    case secd::LOAD_LOCAL_VARIADIC:
      os << "load-local-variadic";
      break;

    case secd::MAKE_CLOSURE:
      os << "make-closure";
      break;

    case secd::MAKE_CONTINUATION:
      os << "make-continuation";
      break;

    case secd::MAKE_MODULE:
      os << "make-module";
      break;

    case secd::POP:
      os << "pop";
      break;

    case secd::PUSH:
      os << "push";
      break;

    case secd::RETURN:
      os << "return";
      break;

    case secd::SELECT:
      os << "select";
      break;

    // case secd::SELECT_TAIL:
    //   os << "select-tail";
    //   break;

    case secd::SET_GLOBAL:
      os << "set-global";
      break;

    case secd::SET_LOCAL:
      os << "set-local";
      break;

    case secd::SET_LOCAL_VARIADIC:
      os << "set-local-variadic";
      break;

    case secd::STOP:
      os << "stop";
      break;
    }

    return os << "\x1b[0m";
  }

  static const auto _apply_               {make<instruction>(secd::APPLY)};
  static const auto _define_              {make<instruction>(secd::DEFINE)};
  static const auto _join_                {make<instruction>(secd::JOIN)};
  static const auto _load_global_         {make<instruction>(secd::LOAD_GLOBAL)};
  static const auto _load_literal_        {make<instruction>(secd::LOAD_LITERAL)};
  static const auto _load_local_          {make<instruction>(secd::LOAD_LOCAL)};
  static const auto _load_local_variadic_ {make<instruction>(secd::LOAD_LOCAL_VARIADIC)};
  static const auto _make_closure_        {make<instruction>(secd::MAKE_CLOSURE)};
  static const auto _make_continuation_   {make<instruction>(secd::MAKE_CONTINUATION)};
  static const auto _make_module_         {make<instruction>(secd::MAKE_MODULE)};
  static const auto _pop_                 {make<instruction>(secd::POP)};
  static const auto _push_                {make<instruction>(secd::PUSH)};
  static const auto _return_              {make<instruction>(secd::RETURN)};
  static const auto _select_              {make<instruction>(secd::SELECT)};
  // static const auto _select_tail_         {make<instruction>(secd::SELECT_TAIL)};
  static const auto _set_global_          {make<instruction>(secd::SET_GLOBAL)};
  static const auto _set_local_           {make<instruction>(secd::SET_LOCAL)};
  static const auto _set_local_variadic_  {make<instruction>(secd::SET_LOCAL_VARIADIC)};
  static const auto _stop_                {make<instruction>(secd::STOP)};
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_INSTRUCTION_HPP

