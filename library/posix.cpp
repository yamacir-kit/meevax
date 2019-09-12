#include <meevax/system/environment.hpp>
#include <meevax/system/native.hpp>

namespace meevax::library
{
  extern "C" NATIVE(library)
  {
    using namespace meevax::system;

    environment library {};

    library.define<native>("dummy", [](auto&&)
    {
      std::cout << "dummy!" << std::endl;
      return unit;
    });

    return make<environment>(library);
  }
} // namespace meevax::library

