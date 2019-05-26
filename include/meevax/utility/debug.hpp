#ifndef INCLUDED_MEEVAX_UTILITY_DEBUG_HPP
#define INCLUDED_MEEVAX_UTILITY_DEBUG_HPP

#include <iomanip>
#include <iostream>

#define SETUP_TRACER \
  static std::size_t meevax_utility_debug_indent {0};

#define INDENT \
  std::string(meevax_utility_debug_indent * 4, ' ')

#define INDENT_RIGHT \
  ++meevax_utility_debug_indent;

#define INDENT_LEFT \
  --meevax_utility_debug_indent;

#define TRACE(TAG) \
  std::cerr << "[" << TAG << "] " << INDENT << " " << std::flush

#endif // INCLUDED_MEEVAX_UTILITY_DEBUG_HPP

