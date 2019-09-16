#include <numeric>

#include <meevax/system/boolean.hpp>
#include <meevax/system/native.hpp>
#include <meevax/system/numerical.hpp>

extern "C" namespace meevax::system
{
  NATIVE(addition)
  {
    return std::accumulate(std::begin(args), std::end(args), make<real>(0), std::plus {});
  }

  NATIVE(multiplication)
  {
    return std::accumulate(std::begin(args), std::end(args), make<real>(1), std::multiplies {});
  }

  NATIVE(subtraction)
  {
    if (length(args) < 2)
    {
      return std::accumulate(std::begin(args), std::end(args), make<real>(0), std::minus {});
    }
    else
    {
      return std::accumulate(std::next(std::begin(args)), std::end(args), *std::begin(args), std::minus {});
    }
  }

  NATIVE(division)
  {
    if (length(args) < 2)
    {
      return std::accumulate(std::begin(args), std::end(args), make<real>(1), std::divides {});
    }
    else
    {
      return std::accumulate(std::next(std::begin(args)), std::end(args), *std::begin(args), std::divides {});
    }
  }

  NATIVE(less)
  {
    return std::invoke(std::less {}, car(args), cadr(args)) ? true_object : false_object;
  }

  NATIVE(less_equal)
  {
    return std::invoke(std::less_equal {}, car(args), cadr(args)) ? true_object : false_object;
  }

  NATIVE(greater)
  {
    return std::invoke(std::greater {}, car(args), cadr(args)) ? true_object : false_object;
  }

  NATIVE(greater_equal)
  {
    return std::invoke(std::greater_equal {}, car(args), cadr(args)) ? true_object : false_object;
  }

  NATIVE(real_)
  {
    return car(args).is<real>() ? true_object : false_object;
  }
} // extern "C"

