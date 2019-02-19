#ifndef INCLUDED_MEEVAX_CONCEPTS_IS_CHARACTER_HPP
#define INCLUDED_MEEVAX_CONCEPTS_IS_CHARACTER_HPP

#include <meevax/concepts/macro.hpp>

namespace meevax::concepts
{
  DEFINE_CONCEPT(is_character);

  DEFINE_CONCEPT_SPECIALIZATON(is_character, char)
  DEFINE_CONCEPT_SPECIALIZATON(is_character, char16_t)
  DEFINE_CONCEPT_SPECIALIZATON(is_character, char32_t)
  DEFINE_CONCEPT_SPECIALIZATON(is_character, wchar_t)

  DEFINE_CONCEPT(is_primitive_character);

  DEFINE_CONCEPT_SPECIALIZATON(is_primitive_character, char)
  DEFINE_CONCEPT_SPECIALIZATON(is_primitive_character, char16_t)
  DEFINE_CONCEPT_SPECIALIZATON(is_primitive_character, char32_t)
  DEFINE_CONCEPT_SPECIALIZATON(is_primitive_character, wchar_t)
};

#endif // INCLUDED_MEEVAX_CONCEPTS_IS_CHARACTER_HPP

