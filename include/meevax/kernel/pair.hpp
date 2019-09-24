#ifndef INCLUDED_MEEVAX_KERNEL_PAIR_HPP
#define INCLUDED_MEEVAX_KERNEL_PAIR_HPP

#include <meevax/kernel/exception.hpp>
#include <meevax/kernel/object.hpp>

namespace meevax::kernel
{
  struct pair
    : public std::pair<object, object>
    , public facade<pair>
  {
    template <typename... Ts>
    explicit constexpr pair(Ts&&... operands)
      : std::pair<object, object> {std::forward<decltype(operands)>(operands)...}
    {}

    pair()
      : std::pair<object, object> {unit, unit}
    {}
  };

  #ifndef NDEBUG
  #define SELECTOR(NAME, INDEX)                                                \
  decltype(auto) NAME(const object& object)                                    \
  {                                                                            \
    if (object)                                                                \
    {                                                                          \
      return std::get<INDEX>(object.dereference());                            \
    }                                                                          \
    else                                                                       \
    {                                                                          \
      throw kernel_error_about_pair {                                          \
        "internal illegal selection rejected"                                  \
      };                                                                       \
    }                                                                          \
  }
  #else
  #define SELECTOR(NAME, INDEX)                                                \
  decltype(auto) NAME(const object& object)                                    \
  {                                                                            \
    return std::get<INDEX>(object.dereference());                              \
  }
  #endif // NDEBUG

  SELECTOR(car, 0)
  SELECTOR(cdr, 1)

  auto operator<<(std::ostream& os, const pair& pare)
    -> decltype(os)
  {
    os << highlight::syntax << "(" << attribute::normal << std::get<0>(pare);

    for (auto object {std::get<1>(pare)}; object; object = cdr(object))
    {
      if (object.is<pair>())
      {
        os << " " << car(object);
      }
      else // iter is the last element of dotted-list.
      {
        os << highlight::syntax << " . " << attribute::normal << object;
      }
    }

    return os << highlight::syntax << ")" << attribute::normal;
  }
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_PAIR_HPP

