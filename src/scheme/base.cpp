#include <scheme/base.hpp>

using namespace meevax::system;

cursor quote(const cursor& exp,
             const cursor&,
             const cursor& continuation)
{
  return cons(LDC, cadr(exp), continuation);
}

