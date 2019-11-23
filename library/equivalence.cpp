#include <meevax/kernel/procedure.hpp>

extern "C" namespace meevax::equivalence
{
  PROCEDURE(equals)
  {
    return
      MEEVAX_BOOLEAN(
        kernel::car(operands) == kernel::cadr(operands));
  }

  PROCEDURE(equivalent)
  {
    if (const kernel::object& object1 {kernel::car(operands)},
                              object2 {kernel::cadr(operands)};
        object1 == object2)
    {
      return kernel::true_object;
    }
    else if (!object1 or !object2)
    {
      return kernel::false_object;
    }
    else
    {
      return MEEVAX_BOOLEAN(object1.equivalent_to(object2));
    }
  }
} // extern "C"

