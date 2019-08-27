#include <meevax/system/environment.hpp>

namespace meevax::example
{
  extern "C" PROCEDURE(export_library)
  {
    using namespace meevax::system;

    environment library {};

    library.global_define<procedure>("hoge", [](auto&& operands)
    {
      std::cerr << "; This message is print by procedure \"hoge\" imported from example library" << std::endl;
      std::cerr << "; Operands are " << operands << std::endl;

      return undefined;
    });

    return library.export_library();
  }
} // namespace meevax::posix

