#ifndef INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
#define INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP

// #include <unordered_map>

#include <meevax/kernel/object.hpp>

namespace meevax::kernel
{
  DERIVE(character, public, std::string)

  std::ostream& operator<<(std::ostream& os, const character& c)
  {
    return os << highlight::simple_datum << "#\\" << static_cast<const std::string&>(c) << attribute::normal;
  }

  extern "C" const object end_of_file;

  // extern "C" std::unordered_map<std::string, object> characters;
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP

