#include <numeric>

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/native.hpp>
#include <meevax/kernel/numerical.hpp>

extern "C" namespace meevax::numerical
{
  NATIVE(addition)
  {
    return std::accumulate(
             std::begin(args), std::end(args),
             kernel::make<kernel::real>(0),
             std::plus {}
           );
  }

  NATIVE(multiplication)
  {
    return std::accumulate(
             std::begin(args), std::end(args),
             kernel::make<kernel::real>(1),
             std::multiplies {}
           );
  }

  NATIVE(subtraction)
  {
    if (kernel::length(args) < 2)
    {
      return std::accumulate(
               std::begin(args), std::end(args),
               kernel::make<kernel::real>(0),
               std::minus {}
             );
    }
    else
    {
      return std::accumulate(
               std::next(std::begin(args)), std::end(args),
               *std::begin(args),
               std::minus {}
             );
    }
  }

  NATIVE(division)
  {
    if (kernel::length(args) < 2)
    {
      return std::accumulate(
               std::begin(args), std::end(args),
               kernel::make<kernel::real>(1),
               std::divides {}
             );
    }
    else
    {
      return std::accumulate(
               std::next(std::begin(args)), std::end(args),
               *std::begin(args),
               std::divides {}
             );
    }
  }

  NATIVE(less)
  {
    return std::invoke(std::less {}, kernel::car(args), kernel::cadr(args)) ? kernel::true_object : kernel::false_object;
  }

  NATIVE(less_equal)
  {
    return std::invoke(std::less_equal {}, kernel::car(args), kernel::cadr(args)) ? kernel::true_object : kernel::false_object;
  }

  NATIVE(greater)
  {
    return std::invoke(std::greater {}, kernel::car(args), kernel::cadr(args)) ? kernel::true_object : kernel::false_object;
  }

  NATIVE(greater_equal)
  {
    return std::invoke(std::greater_equal {}, kernel::car(args), kernel::cadr(args)) ? kernel::true_object : kernel::false_object;
  }

  NATIVE(real_)
  {
    return kernel::car(args).is<kernel::real>() ? kernel::true_object : kernel::false_object;
  }
} // extern "C"

