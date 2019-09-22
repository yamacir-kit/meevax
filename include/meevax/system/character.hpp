#ifndef INCLUDED_MEEVAX_SYSTEM_CHARACTER_HPP
#define INCLUDED_MEEVAX_SYSTEM_CHARACTER_HPP

// #include <unordered_map>

#include <meevax/system/object.hpp>

namespace meevax::system
{
  DERIVE(character, public, std::string)

  std::ostream& operator<<(std::ostream& os, const character& c)
  {
    return os << highlight::simple_datum << "#\\" << static_cast<const std::string&>(c) << attribute::normal;
  }

  extern "C" const object end_of_file;

  // extern "C" std::unordered_map<std::string, object> characters;
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_CHARACTER_HPP

