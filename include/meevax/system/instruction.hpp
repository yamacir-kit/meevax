#ifndef INCLUDED_MEEVAX_SYSTEM_INSTRUCTION_HPP
#define INCLUDED_MEEVAX_SYSTEM_INSTRUCTION_HPP

#include <meevax/system/cursor.hpp>

namespace meevax::system
{
  const cursor LDX    {cursor::bind<std::string>("ldx")};
  const cursor LDC    {cursor::bind<std::string>("ldc")};
  const cursor LDG    {cursor::bind<std::string>("ldg")};
  const cursor LDF    {cursor::bind<std::string>("ldf")};
  const cursor APPLY  {cursor::bind<std::string>("apply")};
  const cursor RETURN {cursor::bind<std::string>("return")};
  const cursor SELECT {cursor::bind<std::string>("select")};
  const cursor JOIN   {cursor::bind<std::string>("join")};
  const cursor POP    {cursor::bind<std::string>("pop")};
  const cursor CAR    {cursor::bind<std::string>("car")};
  const cursor CDR    {cursor::bind<std::string>("cdr")};
  const cursor CONS   {cursor::bind<std::string>("cons")};
  const cursor DEFINE {cursor::bind<std::string>("define")};
  const cursor STOP   {cursor::bind<std::string>("stop")};
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_INSTRUCTION_HPP

