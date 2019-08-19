#include <meevax/system/environment.hpp>

extern "C"
{
  PROCEDURE(dummy)
  {
    std::cerr << "; procedure\t; dummy" << std::endl;
    return meevax::system::true_object;
  }

  PROCEDURE(export_native)
  {
    using namespace meevax::system;

    environment module {};

    module.global_define<procedure>("dummy", dummy);

    return make<environment>(module);
  }
} // extern "C"

