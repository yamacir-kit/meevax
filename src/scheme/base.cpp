#include <scheme/base.hpp>

// From R7RS 6.2.6. Numerical operations
//
//   (+ z1 ...)                                                        procedure
//   (* z1 ...)                                                        procedure
//
// These procedures return the sum or product of their arguments.
//
PROCEDURE(plus)
{
  using namespace meevax::system;
  return std::accumulate(std::begin(args), std::end(args), make<number>(0), std::plus {});
}
//
PROCEDURE(multiplies)
{
  using namespace meevax::system;
  return std::accumulate(std::begin(args), std::end(args), make<number>(1), std::multiplies {});
}

// From R7RS 6.2.6. Numerical operations
//
//   (- z)                                                             procedure
//   (- z1 z2 ...)                                                     procedure
//   (/ z)                                                             procedure
//   (/ z1 z2 ...)                                                     procedure
//
// With two or more arguments, these procedures return the difference or
// quotient of their arguments, associating to the left. With one argumet,
// however, they return the additive or multiplicative inverse of their argument.
//
// It is an error if any argument of / other than the first is an exact zero. If
// the first argument is an exact zero, an implementation may return an exact
// zero unless one of the other arguments is a NaN.
//
PROCEDURE(minus)
{
  using namespace meevax::system;

  if (std::distance(std::begin(args), std::end(args)) < 2)
  {
    return std::accumulate(std::begin(args), std::end(args), make<number>(0), std::minus {});
  }
  else
  {
    return std::accumulate(std::next(std::begin(args)), std::end(args), *std::begin(args), std::minus {});
  }
}
//
PROCEDURE(divides)
{
  using namespace meevax::system;
  return std::accumulate(std::begin(args), std::end(args), make<number>(1), std::divides {});
}

PROCEDURE(symbolic_equal)
{
  using namespace meevax::system;
  return car(args) == cadr(args) ? true_v : false_v;
}

PROCEDURE(is_pair)
{
  using namespace meevax::system;

  for (const auto& each : args)
  {
    if (not each or not each.is<pair>())
    {
      return false_v;
    }
  }

  return true_v;
}

