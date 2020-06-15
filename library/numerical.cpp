#include <numeric>

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/numerical.hpp>
#include <meevax/kernel/procedure.hpp>

extern "C" namespace meevax::numerical
{
  PROCEDURE(addition)
  {
    return
      MEEVAX_API_FOLD(
        xs, kernel::make<kernel::real>(0), std::plus {});
  }

  PROCEDURE(multiplication)
  {
    return
      MEEVAX_API_FOLD(
        xs, kernel::make<kernel::real>(1), std::multiplies {});
  }

  PROCEDURE(subtraction)
  {
    if (kernel::length(xs) < 2)
    {
      return
        MEEVAX_API_FOLD(
          xs, kernel::make<kernel::real>(0), std::minus {});
    }
    else
    {
      return std::accumulate(
               std::next(std::begin(xs)), std::end(xs),
               *std::begin(xs),
               std::minus {}
             );
    }
  }

  PROCEDURE(division)
  {
    if (kernel::length(xs) < 2)
    {
      return
        MEEVAX_API_FOLD(
          xs, kernel::make<kernel::real>(1), std::divides {});
    }
    else
    {
      return std::accumulate(
               std::next(std::begin(xs)), std::end(xs),
               *std::begin(xs),
               std::divides {}
             );
    }
  }

  PROCEDURE(less)
  {
    return
      kernel::convert(
        MEEVAX_BINARY_OPERATION(std::less {}));
  }

  PROCEDURE(less_equal)
  {
    return
      kernel::convert(
        MEEVAX_BINARY_OPERATION(std::less_equal {}));
  }

  PROCEDURE(greater)
  {
    return
      kernel::convert(
        MEEVAX_BINARY_OPERATION(std::greater {}));
  }

  PROCEDURE(greater_equal)
  {
    return
      kernel::convert(
        MEEVAX_BINARY_OPERATION(std::greater_equal {}));
  }

  PROCEDURE(real_)
  {
    return
      kernel::convert(
        kernel::car(xs).is<kernel::real>());
  }
} // extern "C"

