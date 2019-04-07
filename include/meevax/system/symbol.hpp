#ifndef INCLUDED_MEEVAX_SYSTEM_INSTRUCTION_HPP
#define INCLUDED_MEEVAX_SYSTEM_INSTRUCTION_HPP

#include <meevax/system/cursor.hpp>

namespace meevax::system
{
  const cursor LDX    {make<std::string>("ldx")};
  const cursor LDC    {make<std::string>("ldc")};
  const cursor LDG    {make<std::string>("ldg")};
  const cursor LDF    {make<std::string>("ldf")};
  const cursor APPLY  {make<std::string>("apply")};
  const cursor RETURN {make<std::string>("return")};
  const cursor SELECT {make<std::string>("select")};
  const cursor JOIN   {make<std::string>("join")};
  const cursor POP    {make<std::string>("pop")};
  const cursor CAR    {make<std::string>("car")};
  const cursor CDR    {make<std::string>("cdr")};
  const cursor CONS   {make<std::string>("cons")};
  const cursor DEFINE {make<std::string>("define")};
  const cursor STOP   {make<std::string>("stop")};
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_INSTRUCTION_HPP

