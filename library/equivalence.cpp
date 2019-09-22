#include <meevax/system/native.hpp>

extern "C"
{
  NATIVE(address_equal)
  {
    using namespace meevax::system;

    return car(args) == cadr(args) ? true_object : false_object;
  }

  NATIVE(value_equal)
  {
    using namespace meevax::system;

    if (const object& object1 {car(args)}, object2 {cadr(args)}; object1 == object2)
    {
      return true_object;
    }
    else if (!object1 or !object2)
    {
      return false_object;
    }
    else
    {
      return object1.equals(object2) ? true_object : false_object;
    }
  }
} // extern "C"

