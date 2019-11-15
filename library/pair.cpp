#include <meevax/kernel/procedure.hpp>

extern "C" namespace meevax::pair
{
  PROCEDURE(car)
  {
    return kernel::caar(operands);
  }

  PROCEDURE(cdr)
  {
    return kernel::cdar(operands);
  }

  PROCEDURE(cons)
  {
    return kernel::cons(
             kernel::car(operands),
             kernel::cadr(operands)
           );
  }

  PROCEDURE(pair_)
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

