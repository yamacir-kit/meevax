#ifndef INCLUDED_MEEVAX_CONCEPTS_IS_CHARACTER_HPP
#define INCLUDED_MEEVAX_CONCEPTS_IS_CHARACTER_HPP

#include <meevax/concepts/macro.hpp>

namespace meevax::concepts
{
  CONCEPT(is_character)
  VARIADIC_CONCEPT(are_characters, is_character)

  CONCEPT_SPECIALIZATION(is_character, char)
  CONCEPT_SPECIALIZATION(is_character, char16_t)
  CONCEPT_SPECIALIZATION(is_character, char32_t)
  CONCEPT_SPECIALIZATION(is_character, wchar_t)

  CONCEPT(is_primitive_character)
  VARIADIC_CONCEPT(are_primitive_characters, is_primitive_character)

  CONCEPT_SPECIALIZATION(is_primitive_character, char)
  CONCEPT_SPECIALIZATION(is_primitive_character, char16_t)
  CONCEPT_SPECIALIZATION(is_primitive_character, char32_t)
  CONCEPT_SPECIALIZATION(is_primitive_character, wchar_t)
} // namespace meevax::concepts

#endif // INCLUDED_MEEVAX_CONCEPTS_IS_CHARACTER_HPP

