#ifndef INCLUDED_MEEVAX_CORE_BOOLEAN_HPP
#define INCLUDED_MEEVAX_CORE_BOOLEAN_HPP

#include <iostream>
#include <type_traits>

#include <meevax/core/pair.hpp>

namespace meevax::core
{
  const cursor true_v {cursor::bind<std::true_type>()};
  const cursor false_v {cursor::bind<std::false_type>()};

  template <auto Value>
  std::ostream& operator<<(std::ostream& os, std::bool_constant<Value>)
  {
    return os << "\x1b[36m#" << std::boolalpha << Value << "\x1b[0m";
  }
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_BOOLEAN_HPP

