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

  std::ostream& operator<<(std::ostream& os, const exception& exception)
  {
    return os << "\x1b[31m#<exception \"" << exception.what() << "\">\x1b[0m";
  }

  struct error
    : public exception
  {
    template <typename... Ts>
    constexpr error(Ts&&... args)
      : exception {std::forward<Ts>(args)...}
    {}
  };

  std::ostream& operator<<(std::ostream& os, const error& error)
  {
    return os << "\x1b[31m#<error \"" << error.what() << "\">\x1b[0m";
  }

  struct warning
    : public exception
  {
    template <typename... Ts>
    constexpr warning(Ts&&... args)
      : exception {std::forward<Ts>(args)...}
    {}
  };

  std::ostream& operator<<(std::ostream& os, const warning& warning)
  {
    return os << "\x1b[33m#<warning \"" << warning.what() << "\">\x1b[0m";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP

