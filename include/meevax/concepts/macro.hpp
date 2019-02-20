#ifndef INCLUDED_MEEVAX_CONCEPTS_MACRO_HPP
#define INCLUDED_MEEVAX_CONCEPTS_MACRO_HPP

#include <type_traits>

#define REQUIRES(...) \
typename = typename std::enable_if< \
                      std::conjunction<__VA_ARGS__>::value \
                    >::type

#define CONCEPT(NAME) \
template <typename...> \
struct NAME \
  : public std::false_type \
{};

#define VARIADIC_CONCEPT(NAME, PREDICATE) \
template <typename... Ts> \
struct NAME \
  : public std::conjunction< \
             PREDICATE<typename std::decay<Ts>::type>... \
           > \
{};

#define CONCEPT_SPECIALIZATION(NAME, ...) \
template <> \
struct NAME<__VA_ARGS__> \
  : public std::true_type \
{};

#endif // INCLUDED_MEEVAX_CONCEPTS_MACRO_HPP

