#include <numeric>

#include <meevax/system/boolean.hpp>
#include <meevax/system/native.hpp>
#include <meevax/system/numerical.hpp>

extern "C" namespace meevax::numerical
{
  NATIVE(addition)
  {
    return std::accumulate(
             std::begin(args), std::end(args),
             system::make<system::real>(0),
             std::plus {}
           );
  }

  NATIVE(multiplication)
  {
    return std::accumulate(
             std::begin(args), std::end(args),
             system::make<system::real>(1),
             std::multiplies {}
           );
  }

  NATIVE(subtraction)
  {
    if (system::length(args) < 2)
    {
      return std::accumulate(
               std::begin(args), std::end(args),
               system::make<system::real>(0),
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
    if (system::length(args) < 2)
    {
      return std::accumulate(
               std::begin(args), std::end(args),
               system::make<system::real>(1),
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
    return std::invoke(std::less {}, system::car(args), system::cadr(args)) ? system::true_object : system::false_object;
  }

  NATIVE(less_equal)
  {
    return std::invoke(std::less_equal {}, system::car(args), system::cadr(args)) ? system::true_object : system::false_object;
  }

  NATIVE(greater)
  {
    return std::invoke(std::greater {}, system::car(args), system::cadr(args)) ? system::true_object : system::false_object;
  }

  NATIVE(greater_equal)
  {
    return std::invoke(std::greater_equal {}, system::car(args), system::cadr(args)) ? system::true_object : system::false_object;
  }

  NATIVE(real_)
  {
    return system::car(args).is<system::real>() ? system::true_object : system::false_object;
  }
} // extern "C"

