#ifndef INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
#define INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP

#include <unordered_map>

#include <meevax/kernel/object.hpp>

namespace meevax::kernel
{
  struct character
    : public std::string // TODO convert std::u8string in future.
  {
    using identity = character;

    const std::string external_representation;

    explicit character(const char ascii)
      : std::string {ascii}
    {}

    explicit character(
      const std::string& unicode,
      const std::string& external_representation = {})
      : std::string {unicode}
      , external_representation {external_representation}
    {}

    friend auto operator<<(std::ostream& os, const identity& c)
      -> decltype(os)
    {
      return os << highlight::datum << "#\\"
                << (std::empty(c.external_representation)
                      ? static_cast<std::string>(c)
                      : c.external_representation)
                << attribute::normal;
    }
  };

  extern "C" const std::unordered_map<std::string, object> characters;
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP

