#include <meevax/system/environment.hpp>
#include <meevax/system/procedure.hpp>

namespace meevax::library
{
  extern "C" PROCEDURE(library)
  {
    using namespace meevax::system;

    environment library {};

    library.define<procedure>("pair", [](auto&&)
    {
      return unit;
    });

    return make<environment>(library);
  }
} // namespace meevax::library

