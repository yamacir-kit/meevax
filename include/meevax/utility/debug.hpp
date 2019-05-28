#ifndef INCLUDED_MEEVAX_UTILITY_DEBUG_HPP
#define INCLUDED_MEEVAX_UTILITY_DEBUG_HPP

#include <iomanip>
#include <iostream>

static std::size_t depth {0};

#define TRACE(X) \
  std::cerr << "; " << X << "\t; " << std::string(depth * 4, ' ') << std::flush

#define NEST_IN  ++depth
#define NEST_OUT --depth, TRACE("compile") << ")" << std::endl

#endif // INCLUDED_MEEVAX_UTILITY_DEBUG_HPP

