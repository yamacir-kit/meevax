#ifndef INCLUDED_MEEVAX_SYSTEM_BOOLEAN_HPP
#define INCLUDED_MEEVAX_SYSTEM_BOOLEAN_HPP

#include <iomanip>

#include <meevax/system/cursor.hpp>

namespace meevax::system
{
  const cursor true_v {make<std::true_type>()};
  const cursor false_v {make<std::false_type>()};

  template <bool Value>
  std::ostream& operator<<(std::ostream& os, std::bool_constant<Value>)
  {
    return os << "\x1b[36m#" << std::boolalpha << Value << "\x1b[0m";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_BOOLEAN_HPP

