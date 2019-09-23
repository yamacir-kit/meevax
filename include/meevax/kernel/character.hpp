#ifndef INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
#define INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP

#include <unordered_map>

#include <meevax/kernel/object.hpp>

namespace meevax::kernel
{
  struct character
    : public std::string
  {
    const std::string external_repsesentaion;

    character(const char ascii)
      : std::string {ascii}
    {}

    character(const std::string& unicode)
      : std::string {unicode}
      // , external_repsesentaion {unicode} // XXX Waste of memory
    {}

    character(const std::string& unicode,
              const std::string& external_repsesentaion)
      : std::string {unicode}
      , external_repsesentaion {external_repsesentaion}
    {}
  };

  std::ostream& operator<<(std::ostream& os, const character& c)
  {
    return os << highlight::simple_datum << "#\\"
              << (c.external_repsesentaion.empty() ? static_cast<const std::string&>(c) : c.external_repsesentaion)
              << attribute::normal;
  }

  // NATIVE(write_character)
  // {
  //   port << car(operands).as<std::string>();
  // }

  // TODO Provide user-defined character-name?
  extern "C" const std::unordered_map<std::string, object> characters;
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP

