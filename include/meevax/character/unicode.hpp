#ifndef INCLUDED_MEEVAX_CHARACTER_UNICODE_HPP
#define INCLUDED_MEEVAX_CHARACTER_UNICODE_HPP

#include <string>

#include <meevax/concepts/is_character.hpp>

namespace meevax::character
{
  template <auto N>
  class unicode;

  template <>
  class unicode<8>
    : public std::string
  {};

  CONCEPT_SPECIALIZATION(is_character, unicode<8>)
} // namespace meevax::character

#endif // INCLUDED_MEEVAX_CHARACTER_UNICODE_HPP

