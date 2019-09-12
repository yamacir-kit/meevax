#ifndef INCLUDED_MEEVAX_SYSTEM_BOOLEAN_HPP
#define INCLUDED_MEEVAX_SYSTEM_BOOLEAN_HPP

#include <meevax/system/object.hpp>

namespace meevax::system
{
  extern "C" const object true_object, false_object;

  template <auto Value>
  std::ostream& operator<<(std::ostream& os, std::bool_constant<Value>)
  {
    return os << color::simple_datum << "#" << std::boolalpha << Value << color::normal;
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_BOOLEAN_HPP

