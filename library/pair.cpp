#include <meevax/kernel/native.hpp>

extern "C" namespace meevax::pair
{
  NATIVE(car)
  {
    return kernel::caar(args);
  }

  NATIVE(cdr)
  {
    return kernel::cdar(args);
  }

  NATIVE(cons)
  {
    return kernel::cons(kernel::car(args), kernel::cadr(args));
  }

  NATIVE(pair_)
  {
    for (const auto& each : args)
    {
      if (not each or not each.is<kernel::pair>())
      {
        return kernel::false_object;
      }
    }

    return kernel::true_object;
  }
} // extern "C"

