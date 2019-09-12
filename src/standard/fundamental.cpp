#include <meevax/system/native.hpp>

extern "C"
{
  NATIVE(car)
  {
    using meevax::system::car;
    using meevax::system::cdr;
    return caar(args);
  }

  NATIVE(cdr)
  {
    using meevax::system::car;
    using meevax::system::cdr;
    return cdar(args);
  }

  NATIVE(cons)
  {
    using meevax::system::car;
    using meevax::system::cdr;
    using meevax::system::cons;
    return cons(car(args), cadr(args));
  }
} // extern "C"

