#include <numeric>

#include <meevax/system/boolean.hpp>
#include <meevax/system/number.hpp>
#include <meevax/system/procedure.hpp>
#include <meevax/system/srfi-1.hpp>

extern "C"
{
  // From R7RS 6.2.6. Numerical operations
  //
  //   (+ z1 ...)                                                      procedure
  //   (* z1 ...)                                                      procedure
  //
  // These procedures return the sum or product of their arguments.
  //
  PROCEDURE(addition)
  {
    using namespace meevax::system;
    return std::accumulate(std::begin(args), std::end(args), make<number>(0), std::plus {});
  }
  //
  PROCEDURE(multiplication)
  {
    using namespace meevax::system;
    return std::accumulate(std::begin(args), std::end(args), make<number>(1), std::multiplies {});
  }

  // From R7RS 6.2.6. Numerical operations
  //
  //   (- z)                                                           procedure
  //   (- z1 z2 ...)                                                   procedure
  //   (/ z)                                                           procedure
  //   (/ z1 z2 ...)                                                   procedure
  //
  // With two or more arguments, these procedures return the difference or
  // quotient of their arguments, associating to the left. With one argumet,
  // however, they return the additive or multiplicative inverse of their
  // argument.
  //
  // It is an error if any argument of / other than the first is an exact zero.
  // If the first argument is an exact zero, an implementation may return an
  // exact zero unless one of the other arguments is a NaN.
  //
  PROCEDURE(subtraction)
  {
    using namespace meevax::system;

    if (length(args) < 2)
    {
      return std::accumulate(std::begin(args), std::end(args), make<number>(0), std::minus {});
    }
    else
    {
      return std::accumulate(std::next(std::begin(args)), std::end(args), *std::begin(args), std::minus {});
    }
  }
  //
  PROCEDURE(division)
  {
    using namespace meevax::system;
    return std::accumulate(std::begin(args), std::end(args), make<number>(1), std::divides {});
  }

  PROCEDURE(less)
  {
    using namespace meevax::system;
    return car(args).as<number>() < cadr(args).as<number>() ? true_v : false_v;
  }

  PROCEDURE(greater)
  {
    using namespace meevax::system;
    return car(args).as<number>() > cadr(args).as<number>() ? true_v : false_v;
  }
} // extern "C"

