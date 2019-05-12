#include <scheme/base.hpp>

using namespace meevax::system;

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

cursor divide(const cursor& args)
{
  return std::accumulate(std::begin(args), std::end(args), make<number>(1), std::divides {});
}

cursor minus(const cursor& args)
{
  if (std::distance(std::begin(args), std::end(args)) < 2)
  {
    return std::accumulate(std::begin(args), std::end(args), make<number>(0), std::minus {});
  }
  else
  {
    return std::accumulate(std::next(std::begin(args)), std::end(args), *std::begin(args), std::minus {});
  }
}

cursor multiply(const cursor& args)
{
  return std::accumulate(std::begin(args), std::end(args), make<number>(1), std::multiplies {});
}

cursor plus(const cursor& args)
{
  return std::accumulate(std::begin(args), std::end(args), make<number>(0), std::plus {});
}

