#include <scheme/base.hpp>

using namespace meevax::system;

PROCEDURE(eq)
{
  return car(args) == cadr(args) ? true_v : false_v;
}

PROCEDURE(is_pair)
{
  for (const auto& each : args)
  {
    if (not each or not each.is<pair>())
    {
      return false_v;
    }
  }

  return true_v;
}

PROCEDURE(divide)
{
  return std::accumulate(std::begin(args), std::end(args), make<number>(1), std::divides {});
}

PROCEDURE(minus)
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

PROCEDURE(multiply)
{
  return std::accumulate(std::begin(args), std::end(args), make<number>(1), std::multiplies {});
}

PROCEDURE(plus)
{
  return std::accumulate(std::begin(args), std::end(args), make<number>(0), std::plus {});
}

