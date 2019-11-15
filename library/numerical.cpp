#include <numeric>

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/numerical.hpp>
#include <meevax/kernel/procedure.hpp>

extern "C" namespace meevax::numerical
{
  PROCEDURE(addition)
  {
    return std::accumulate(
             std::begin(operands), std::end(operands),
             kernel::make<kernel::real>(0),
             std::plus {}
           );
  }

  PROCEDURE(multiplication)
  {
    return std::accumulate(
             std::begin(operands), std::end(operands),
             kernel::make<kernel::real>(1),
             std::multiplies {}
           );
  }

  PROCEDURE(subtraction)
  {
    if (kernel::length(operands) < 2)
    {
      return std::accumulate(
               std::begin(operands), std::end(operands),
               kernel::make<kernel::real>(0),
               std::minus {}
             );
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
      return std::accumulate(
               std::begin(operands), std::end(operands),
               kernel::make<kernel::real>(1),
               std::divides {}
             );
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
    return std::invoke(std::less {}, kernel::car(operands), kernel::cadr(operands)) ? kernel::true_object : kernel::false_object;
  }

  PROCEDURE(less_equal)
  {
    return std::invoke(std::less_equal {}, kernel::car(operands), kernel::cadr(operands)) ? kernel::true_object : kernel::false_object;
  }

  PROCEDURE(greater)
  {
    return std::invoke(std::greater {}, kernel::car(operands), kernel::cadr(operands)) ? kernel::true_object : kernel::false_object;
  }

  PROCEDURE(greater_equal)
  {
    return std::invoke(std::greater_equal {}, kernel::car(operands), kernel::cadr(operands)) ? kernel::true_object : kernel::false_object;
  }

  PROCEDURE(real_)
  {
    return kernel::car(operands).is<kernel::real>() ? kernel::true_object : kernel::false_object;
  }
} // extern "C"

