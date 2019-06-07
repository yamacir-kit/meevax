#ifndef INCLUDED_MEEVAX_SYSTEM_BOOLEAN_HPP
#define INCLUDED_MEEVAX_SYSTEM_BOOLEAN_HPP

#include <meevax/system/pair.hpp>

namespace meevax::system
{
  extern "C" const objective _true_;
  extern "C" const objective _false_;

  template <auto Value>
  std::ostream& operator<<(std::ostream& os, std::bool_constant<Value>)
  {
    return os << "\x1b[36m#" << std::boolalpha << Value << "\x1b[0m";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_BOOLEAN_HPP

