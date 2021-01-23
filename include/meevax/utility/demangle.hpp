#ifndef INCLUDED_MEEVAX_UTILITY_DEMANGLE_HPP
#define INCLUDED_MEEVAX_UTILITY_DEMANGLE_HPP

#include <string>
#include <typeinfo>

#include <boost/cstdlib.hpp>

namespace meevax
{
inline namespace utility
{
  auto demangle(char const* name) -> std::string;

  auto demangle(std::type_info const&) -> std::string;
} // namespace utility
} // namespace meevax

#endif // INCLUDED_MEEVAX_UTILITY_DEMANGLE_HPP
