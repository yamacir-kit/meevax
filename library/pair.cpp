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
} // extern "C"

