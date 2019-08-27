#include <meevax/system/environment.hpp>
#include <meevax/system/procedure.hpp>

namespace meevax::library
{
  extern "C" PROCEDURE(library)
  {
    using namespace meevax::system;

    environment library {};

    library.define<procedure>("cons", [](auto&& operands)
    {
      return cons(car(operands), cadr(operands));
    });

    library.define<procedure>("car", [](auto&& operands)
    {
      return caar(operands);
    });

    library.define<procedure>("cdr", [](auto&& operands)
    {
      return cdar(operands);
    });

    return make<environment>(library);
  }
} // namespace meevax::library

