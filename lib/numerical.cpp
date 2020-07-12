#include <numeric>

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/numerical.hpp>
#include <meevax/kernel/procedure.hpp>

extern "C" namespace meevax { inline namespace numerical
{
  PROCEDURE(addition)
  {
    #if __cpp_deduction_guides
    return MEEVAX_API_FOLD(xs, kernel::make<kernel::integer>(0), std::plus {});
    #else
    return MEEVAX_API_FOLD(xs, kernel::make<kernel::integer>(0), std::plus<kernel::object>());
    #endif
  }

  PROCEDURE(multiplication)
  {
    #if __cpp_deduction_guides
    return MEEVAX_API_FOLD(xs, kernel::make<kernel::integer>(1), std::multiplies {});
    #else
    return MEEVAX_API_FOLD(xs, kernel::make<kernel::integer>(1), std::multiplies<kernel::object>());
    #endif
  }

  PROCEDURE(subtraction)
  {
    if (kernel::length(xs) < 2)
    {
      #if __cpp_deduction_guides
      return MEEVAX_API_FOLD(xs, kernel::make<kernel::integer>(0), std::minus {});
      #else
      return MEEVAX_API_FOLD(xs, kernel::make<kernel::integer>(0), std::minus<kernel::object>());
      #endif
    }
    else
    {
      auto iter { std::begin(xs) };

      #if __cpp_deduction_guides
      return std::accumulate(std::next(iter), std::end(xs), *iter, std::minus {});
      #else
      return std::accumulate(std::next(iter), std::end(xs), *iter, std::minus<kernel::object>());
      #endif
    }
  }

  PROCEDURE(division)
  {
    if (kernel::length(xs) < 2)
    {
      #if __cpp_deduction_guides
      return MEEVAX_API_FOLD(xs, kernel::make<kernel::integer>(1), std::divides {});
      #else
      return MEEVAX_API_FOLD(xs, kernel::make<kernel::integer>(1), std::divides<kernel::object>());
      #endif
    }
    else
    {
      auto iter { std::begin(xs) };

      #if __cpp_deduction_guides
      return std::accumulate(std::next(iter), std::end(xs), *iter, std::divides {});
      #else
      return std::accumulate(std::next(iter), std::end(xs), *iter, std::divides<kernel::object>());
      #endif
    }
  }

  PROCEDURE(real_)
  {
    return
      kernel::convert(
        kernel::car(xs).is<kernel::real>());
  }
}} // extern "C"
