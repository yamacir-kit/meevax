#ifndef INCLUDED_MEEVAX_LIBRARY_POSIX_HPP
#define INCLUDED_MEEVAX_LIBRARY_POSIX_HPP

#include <meevax/system/procedure.hpp>

namespace meevax::posix
{
  extern "C" PROCEDURE(export_library);

  PROCEDURE(dummy)
  {
    std::cerr << "; procedure\t; dummy" << std::endl;
    return meevax::system::true_object;
  }
} // namespace meevax::posix

#endif // INCLUDED_MEEVAX_LIBRARY_POSIX_HPP

