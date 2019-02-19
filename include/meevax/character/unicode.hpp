#ifndef INCLUDED_MEEVAX_CHARACTER_UNICODE_HPP
#define INCLUDED_MEEVAX_CHARACTER_UNICODE_HPP

#include <string>

namespace meevax::character
{
  template <auto N>
  class unicode;

  template <>
  class unicode<8>
    : public std::string
  {};
} // namespace meevax::character

#endif // INCLUDED_MEEVAX_CHARACTER_UNICODE_HPP

