#include <meevax/kernel/native.hpp>

extern "C" namespace meevax::equivalence
{
  NATIVE(address_equal)
  {
    return kernel::car(operands) == kernel::cadr(operands) ? kernel::true_object : kernel::false_object;
  }

  NATIVE(value_equal)
  {
    if (const kernel::object& object1 {kernel::car(operands)},
                              object2 {kernel::cadr(operands)}; object1 == object2)
    {
      return kernel::true_object;
    }
    else if (!object1 or !object2)
    {
      return kernel::false_object;
    }
    else
    {
      return object1.equals(object2) ? kernel::true_object : kernel::false_object;
    }
  }
} // extern "C"

