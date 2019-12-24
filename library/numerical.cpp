#include <numeric>

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/numerical.hpp>
#include <meevax/kernel/procedure.hpp>

extern "C" namespace meevax::numerical
{
  PROCEDURE(addition)
  {
    return
      MEEVAX_FOLD_ARGUMENTS(
        kernel::make<kernel::real>(0), std::plus {});
  }

  PROCEDURE(multiplication)
  {
    return
      MEEVAX_FOLD_ARGUMENTS(
        kernel::make<kernel::real>(1), std::multiplies {});
  }

  PROCEDURE(subtraction)
  {
    if (kernel::length(operands) < 2)
    {
      return
        MEEVAX_FOLD_ARGUMENTS(
          kernel::make<kernel::real>(0), std::minus {});
    }
    else
    {
      return std::accumulate(
               std::next(std::begin(operands)), std::end(operands),
               *std::begin(operands),
               std::minus {}
             );
    }
  }

  PROCEDURE(division)
  {
    if (kernel::length(operands) < 2)
    {
      return
        MEEVAX_FOLD_ARGUMENTS(
          kernel::make<kernel::real>(1), std::divides {});
    }
    else
    {
      return std::accumulate(
               std::next(std::begin(operands)), std::end(operands),
               *std::begin(operands),
               std::divides {}
             );
    }
  }

  PROCEDURE(less)
  {
    return
      MEEVAX_API_BOOLEAN(
        MEEVAX_BINARY_OPERATION(std::less {}));
  }

  PROCEDURE(less_equal)
  {
    return
      MEEVAX_API_BOOLEAN(
        MEEVAX_BINARY_OPERATION(std::less_equal {}));
  }

  PROCEDURE(greater)
  {
    return
      MEEVAX_API_BOOLEAN(
        MEEVAX_BINARY_OPERATION(std::greater {}));
  }

  PROCEDURE(greater_equal)
  {
    return
      MEEVAX_API_BOOLEAN(
        MEEVAX_BINARY_OPERATION(std::greater_equal {}));
  }

  PROCEDURE(real_)
  {
    return
      MEEVAX_API_BOOLEAN(
        kernel::car(operands).is<kernel::real>());
  }
} // extern "C"

