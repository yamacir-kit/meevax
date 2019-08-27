#include <meevax/library/pair.hpp>

#include <meevax/system/environment.hpp>

namespace meevax::pair
{
  extern "C" PROCEDURE(export_library)
  {
    using namespace meevax::system;

    environment library {};

    library.define<procedure>("pair", [](auto&&)
    {
      return unit;
    });

    return make<environment>(library);
  }
} // namespace meevax::posix

