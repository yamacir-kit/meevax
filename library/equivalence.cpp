#include <meevax/kernel/procedure.hpp>

extern "C" namespace meevax::equivalence
{
  PROCEDURE(equals)
  {
    return
      kernel::convert(
        kernel::car(operands) == kernel::cadr(operands));
  }

  PROCEDURE(equivalent)
  {
    if (const kernel::object object1 {kernel::car(operands)},
                             object2 {kernel::cadr(operands)};
        object1 == object2)
    {
      return kernel::t;
    }
    else if (not object1 and not object2)
    {
      return kernel::t;
    }
    else if (not object1 or not object2)
    {
      return kernel::f;
    }
    else
    {
      return kernel::convert(object1.equivalent_to(object2));
    }
  }
} // extern "C"

