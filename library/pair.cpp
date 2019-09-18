#include <meevax/system/native.hpp>

extern "C"
{
  NATIVE(car)
  {
    return meevax::system::caar(args);
  }

  NATIVE(cdr)
  {
    return meevax::system::cdar(args);
  }

  NATIVE(cons)
  {
    return meevax::system::cons(meevax::system::car(args), meevax::system::cadr(args));
  }

  NATIVE(pair_)
  {
    using namespace meevax::system;

    for (const auto& each : args)
    {
      if (not each or not each.is<pair>())
      {
        return false_object;
      }
    }

    return true_object;
  }
} // extern "C"

