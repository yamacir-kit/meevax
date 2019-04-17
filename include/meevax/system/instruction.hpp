#ifndef INCLUDED_MEEVAX_SYSTEM_INSTRUCTION_HPP
#define INCLUDED_MEEVAX_SYSTEM_INSTRUCTION_HPP

#include <iomanip>
#include <iostream> // std::ostream
#include <string>
#include <utility>

#include <meevax/system/cursor.hpp>

namespace meevax::system
{
  struct instruction
  {
    enum class secd
    {
      APPLY,
      CAR,
      CDR,
      CONS,
      DEFINE,
      JOIN,
      LDC, // load constant
      LDF, // load function
      LDG, // load global
      LDX, // load local
      POP,
      RETURN,
      SELECT,
      STOP,
    } const code;

    template <typename... Ts>
    instruction(Ts&&... args)
      : code {std::forward<Ts>(args)...}
    {}
  };

  std::ostream& operator<<(std::ostream& os, const instruction& instruction)
  {
    os << "\x1b[32m";

    // os << "\\x" << std::hex << std::setw(2) << std::setfill('0') << static_cast<int>(instruction.code);

    switch (instruction.code)
    {
    case instruction::secd::APPLY:  os << "apply";  break;
    case instruction::secd::CAR:    os << "car";    break;
    case instruction::secd::CDR:    os << "cdr";    break;
    case instruction::secd::CONS:   os << "cons";   break;
    case instruction::secd::DEFINE: os << "define"; break;
    case instruction::secd::JOIN:   os << "join";   break;
    case instruction::secd::LDC:    os << "ldc";    break;
    case instruction::secd::LDF:    os << "ldf";    break;
    case instruction::secd::LDG:    os << "ldg";    break;
    case instruction::secd::LDX:    os << "ldx";    break;
    case instruction::secd::POP:    os << "pop";    break;
    case instruction::secd::RETURN: os << "return"; break;
    case instruction::secd::SELECT: os << "select"; break;
    case instruction::secd::STOP:   os << "stop";   break;
    }

    return os << "\x1b[0m";
  }

  static const auto APPLY  {make<instruction>(0x00)};
  static const auto CAR    {make<instruction>(0x01)};
  static const auto CDR    {make<instruction>(0x02)};
  static const auto CONS   {make<instruction>(0x03)};
  static const auto DEFINE {make<instruction>(0x04)};
  static const auto JOIN   {make<instruction>(0x05)};
  static const auto LDC    {make<instruction>(0x06)};
  static const auto LDF    {make<instruction>(0x07)};
  static const auto LDG    {make<instruction>(0x08)};
  static const auto LDX    {make<instruction>(0x09)};
  static const auto POP    {make<instruction>(0x0a)};
  static const auto RETURN {make<instruction>(0x0b)};
  static const auto SELECT {make<instruction>(0x0c)};
  static const auto STOP   {make<instruction>(0x0d)};
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_INSTRUCTION_HPP

