#ifndef INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP
#define INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP

#include <stdexcept> // std::runtime_error

#include <meevax/system/cursor.hpp>

namespace meevax::system
{
  struct exception
    : public std::runtime_error
  {
    template <typename... Ts>
    constexpr exception(Ts&&... args)
      : std::runtime_error {std::forward<Ts>(args)...}
    {}
  };

  std::ostream& operator<<(std::ostream& os, const exception& error)
  {
    return os << "\x1b[31m#<exception \"" << error.what() << "\">\x1b[0m";
  }

  struct error
    : public exception
  {
    template <typename... Ts>
    constexpr error(Ts&&... args)
      : exception {std::forward<Ts>(args)...}
    {}
  };

  const cursor undefined {make<exception>("undefined")};
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP

