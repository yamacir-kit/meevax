#include <meevax/kernel/procedure.hpp>

extern "C" namespace meevax::pair
{
  PROCEDURE(car)
  {
    return kernel::caar(xs);
  }

  PROCEDURE(cdr)
  {
    return kernel::cdar(xs);
  }

  PROCEDURE(cons)
  {
    return
      kernel::cons(
        kernel::car(xs),
        kernel::cadr(xs));
  }

  PROCEDURE(pair_)
  {
    // for (const auto& each : xs)
    // {
    //   if (not each or not each.is<kernel::pair>())
    //   {
    //     return kernel::f;
    //   }
    // }
    //
    // return t;

    const auto& value {kernel::car(xs)};

    return kernel::convert(value && value.is<kernel::pair>());
  }
} // extern "C"

