#ifndef INCLUDED_MEEVAX_CONCEPTS_IS_CHARACTER_HPP
#define INCLUDED_MEEVAX_CONCEPTS_IS_CHARACTER_HPP

#include <type_traits>

namespace meevax::concepts
{
  template <typename...>
  struct is_character
    : public std::false_type
  {};

  #ifndef DEFINE_CONCEPT_IS_CHARACTER_SPECIALIZATON
  #define DEFINE_CONCEPT_IS_CHARACTER_SPECIALIZATON(...) \
  template <> \
  struct is_character<__VA_ARGS__> \
    : public std::true_type \
  {};

  DEFINE_CONCEPT_IS_CHARACTER_SPECIALIZATON(char)
  DEFINE_CONCEPT_IS_CHARACTER_SPECIALIZATON(char16_t)
  DEFINE_CONCEPT_IS_CHARACTER_SPECIALIZATON(char32_t)
  DEFINE_CONCEPT_IS_CHARACTER_SPECIALIZATON(wchar_t)
};

#endif // INCLUDED_MEEVAX_CONCEPTS_IS_CHARACTER_HPP

