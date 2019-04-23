#include <scheme/base.hpp>

using namespace meevax::system;

cursor quote(const cursor& exp,
             const cursor&,
             const cursor& continuation)
{
  return cons(LDC, cadr(exp), continuation);
}

cursor eq(const cursor& args)
{
  return car(args) == cadr(args) ? true_v : false_v;
}

cursor is_pair(const cursor& args)
{
  for (const cursor& each : args)
  {
    if (not each or not each.is<pair>())
    {
      return false_v;
    }
  }

  return true_v;
}

cursor plus(const cursor& args)
{
  return std::accumulate(args, unit, make<number>(0), std::plus {});
}

cursor multiply(const cursor& args)
{
  return std::accumulate(args, unit, make<number>(1), std::multiplies {});
}

cursor minus(const cursor& args)
{
  if (std::distance(args, unit) < 2)
  {
    return std::accumulate(args, unit, make<number>(0), std::minus {});
  }
  else
  {
    return std::accumulate(cursor {cdr(args)}, unit, car(args), std::minus {});
  }
}

cursor divide(const cursor& args)
{
  return std::accumulate(args, unit, make<number>(1), std::divides {});
}

