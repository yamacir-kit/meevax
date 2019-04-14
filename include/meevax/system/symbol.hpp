#ifndef INCLUDED_MEEVAX_SYSTEM_SYMBOL_HPP
#define INCLUDED_MEEVAX_SYSTEM_SYMBOL_HPP

#include <iostream>
#include <string>
#include <utility>

#include <meevax/system/cursor.hpp>

namespace meevax::system
{
  struct symbol
    : public std::string
  {
    template <typename... Ts>
    constexpr symbol(Ts&&... args)
      : std::string {std::forward<Ts>(args)...}
    {}
  };

  std::ostream& operator<<(std::ostream& os, const symbol& symbol)
  {
    return os << static_cast<const std::string&>(symbol);
  }

  const cursor APPLY  {make<symbol>("apply")};
  const cursor CAR    {make<symbol>("car")};
  const cursor CDR    {make<symbol>("cdr")};
  const cursor CONS   {make<symbol>("cons")};
  const cursor DEFINE {make<symbol>("define")};
  const cursor JOIN   {make<symbol>("join")};
  const cursor LDC    {make<symbol>("ldc")};
  const cursor LDF    {make<symbol>("ldf")};
  const cursor LDG    {make<symbol>("ldg")};
  const cursor LDX    {make<symbol>("ldx")};
  const cursor POP    {make<symbol>("pop")};
  const cursor RETURN {make<symbol>("return")};
  const cursor SELECT {make<symbol>("select")};
  const cursor STOP   {make<symbol>("stop")};
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_SYMBOL_HPP

