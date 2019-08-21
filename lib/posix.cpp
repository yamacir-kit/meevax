#include <meevax/system/environment.hpp>

extern "C"
{
  PROCEDURE(dummy)
  {
    std::cerr << "; procedure\t; dummy" << std::endl;
    return meevax::system::true_object;
  }

  PROCEDURE(define_library)
  {
    using namespace meevax::system;

    environment library {};

    library.global_define<procedure>("dummy", dummy);

    return make<environment>(library);
  }
} // extern "C"

