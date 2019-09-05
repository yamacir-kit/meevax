#ifndef INCLUDED_MEEVAX_SYSTEM_CHARACTER_HPP
#define INCLUDED_MEEVAX_SYSTEM_CHARACTER_HPP

#include <string>
// #include <unordered_map>

#include <meevax/system/object.hpp>
#include <meevax/system/writer.hpp>

namespace meevax::system
{
  DERIVE(character, public, std::string)

  std::ostream& operator<<(std::ostream& os, const character& c)
  {
    return os << color::literal << "#\\" << static_cast<const std::string&>(c) << color::normal;
  }

  // extern "C" std::unordered_map<std::string, object> characters;
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_CHARACTER_HPP

