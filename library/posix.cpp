#include <meevax/library/posix.hpp>
#include <meevax/system/environment.hpp>

namespace meevax::posix
{
  extern "C" PROCEDURE(export_library)
  {
    using namespace meevax::system;

    environment library {};

    library.global_define<procedure>("dummy", dummy);

    return library.export_library();
  }
} // namespace meevax::posix

