#ifndef INCLUDED_MEEVAX_CORE_INSTRUCTION_HPP
#define INCLUDED_MEEVAX_CORE_INSTRUCTION_HPP

#include <meevax/core/pair.hpp>

namespace meevax::core
{
  const cursor LDX  {cursor::bind<symbol>("ldx")};
  const cursor LDC  {cursor::bind<symbol>("ldc")};
  const cursor LDG  {cursor::bind<symbol>("ldg")};
  const cursor LDF  {cursor::bind<symbol>("ldf")};
  const cursor APP  {cursor::bind<symbol>("apply")};
  const cursor RET  {cursor::bind<symbol>("return")};
  const cursor SEL  {cursor::bind<symbol>("select")};
  const cursor JOIN {cursor::bind<symbol>("join")};
  const cursor POP  {cursor::bind<symbol>("pop")};
  const cursor CAR  {cursor::bind<symbol>("car")};
  const cursor CDR  {cursor::bind<symbol>("cdr")};
  const cursor CONS {cursor::bind<symbol>("cons")};
  const cursor DEF  {cursor::bind<symbol>("define")};
  const cursor STOP {cursor::bind<symbol>("stop")};
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_INSTRUCTION_HPP

