#ifndef INCLUDED_MEEVAX_STRING_INDENT_HPP
#define INCLUDED_MEEVAX_STRING_INDENT_HPP

#include <ostream>
#include <string>

namespace meevax
{
  struct indent
  {
    static inline           std::size_t depth         = 0;
    static inline constexpr std::size_t default_width = 2;
    static inline           std::size_t         width = default_width;

    constexpr indent() = default;

    operator std::string() const
    {
      return std::string(depth, ' ');
    }
  };

  auto operator <<(std::ostream & os, indent const& datum) -> std::ostream &;

  auto operator <<(indent &  datum, std::size_t width) -> indent &;
  auto operator <<(indent && datum, std::size_t width) -> indent &;
  auto operator >>(indent &  datum, std::size_t width) -> indent &;
  auto operator >>(indent && datum, std::size_t width) -> indent &;
} // namespace meevax

#endif // INCLUDED_MEEVAX_STRING_INDENT_HPP
