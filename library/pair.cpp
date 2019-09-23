#include <meevax/kernel/native.hpp>

extern "C" namespace meevax::pair
{
  NATIVE(car)
  {
    return kernel::caar(operands);
  }

  NATIVE(cdr)
  {
    return kernel::cdar(operands);
  }

  NATIVE(cons)
  {
    return kernel::cons(
             kernel::car(operands),
             kernel::cadr(operands)
           );
  }

  NATIVE(pair_)
  {
    for (const auto& each : operands)
    {
      if (not each or not each.is<kernel::pair>())
      {
        return kernel::false_object;
      }
    }

    return kernel::true_object;
  }
} // extern "C"

