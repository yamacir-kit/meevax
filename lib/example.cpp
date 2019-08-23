#include <meevax/system/environment.hpp>

namespace meevax::example
{
  extern "C" PROCEDURE(export_library)
  {
    using namespace meevax::system;

    environment library {};

    library.global_define<procedure>("sample-procedure", [](auto&& operands)
    {
      std::cerr << "; This message is print by procedure \"sample-procedure\" imported from example library" << std::endl;

      return undefined;
    });

    return library.export_library();
  }
} // namespace meevax::posix

