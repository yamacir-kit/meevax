#ifndef INCLUDED_MEEVAX_STANDARD_POSIX_HPP
#define INCLUDED_MEEVAX_STANDARD_POSIX_HPP

#include <meevax/system/procedure.hpp>

extern "C" namespace meevax::posix
{
  PROCEDURE(dummy)
  {
    std::cerr << "; procedure\t; dummy" << std::endl;
    return meevax::system::true_object;
  }

  PROCEDURE(export_library);
} // extern "C" namespace meevax::posix

#endif // INCLUDED_MEEVAX_STANDARD_POSIX_HPP

