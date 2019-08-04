#include <meevax/system/pair.hpp>
#include <meevax/system/procedure.hpp>

extern "C"
{
  PROCEDURE(car)
  {
    using meevax::system::car;
    using meevax::system::cdr;
    return caar(args);
  }

  PROCEDURE(cdr)
  {
    using meevax::system::car;
    using meevax::system::cdr;
    return cdar(args);
  }

  PROCEDURE(cons)
  {
    using meevax::system::car;
    using meevax::system::cdr;
    using meevax::system::cons;
    return cons(car(args), cadr(args));
  }
} // extern "C"

