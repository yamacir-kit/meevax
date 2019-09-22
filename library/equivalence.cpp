#include <meevax/system/native.hpp>

extern "C" namespace meevax::equivalence
{
  NATIVE(address_equal)
  {
    return system::car(args) == system::cadr(args) ? system::true_object : system::false_object;
  }

  NATIVE(value_equal)
  {
    if (const system::object& object1 {system::car(args)},
                              object2 {system::cadr(args)}; object1 == object2)
    {
      return system::true_object;
    }
    else if (!object1 or !object2)
    {
      return system::false_object;
    }
    else
    {
      return object1.equals(object2) ? system::true_object : system::false_object;
    }
  }
} // extern "C"

