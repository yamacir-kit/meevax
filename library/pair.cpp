#include <meevax/system/environment.hpp>
#include <meevax/system/native.hpp>

namespace meevax::library
{
  extern "C" NATIVE(library)
  {
    using namespace meevax::system;

    environment library {};

    library.define<native>("cons", [](auto&& operands)
    {
      return cons(car(operands), cadr(operands));
    });

    library.define<native>("car", [](auto&& operands)
    {
      return caar(operands);
    });

    library.define<native>("cdr", [](auto&& operands)
    {
      return cdar(operands);
    });

    return make<environment>(library);
  }
} // namespace meevax::library

