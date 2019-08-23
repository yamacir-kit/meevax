#include <meevax/standard/posix.hpp>
#include <meevax/system/environment.hpp>

extern "C" namespace meevax::posix
{
  PROCEDURE(export_library)
  {
    using namespace meevax::system;

    environment library {};

    library.global_define<procedure>("dummy", dummy);

    return library.export_library();
  }
} // namespace meevax::posix

