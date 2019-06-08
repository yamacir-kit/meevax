#ifndef INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP
#define INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP

#include <sstream>
#include <stdexcept> // std::runtime_error
#include <type_traits> // std::is_constructible

#include <meevax/concepts/requires.hpp>
#include <meevax/utility/perfect_derive.hpp>

// exception
//  |-- error
//  |    `-- syntax_error
//  `-- warning

namespace meevax::system
{
  struct exception
    : public std::runtime_error
  {
    template <typename S, REQUIRES(std::is_constructible<std::string, S>)>
    constexpr exception(S&& s)
      : std::runtime_error {std::forward<S>(s)}
    {}

    template <typename... Ts>
    exception(Ts&&... args)
      : std::runtime_error {to_string(std::forward<Ts>(args)...)}
    {}

    template <typename... Ts>
    static decltype(auto) to_string(Ts&&... args)
    {
      std::stringstream ss {};
      (ss << ... << args);
      return ss.str();
    }
  };

  PERFECT_DERIVE(error, public, exception)
  PERFECT_DERIVE(warning, public, exception)

  std::ostream& operator<<(std::ostream& os, const exception& exception)
  {
    return os << "\x1b[31m#<exception \"" << exception.what() << "\">\x1b[0m";
  }

  std::ostream& operator<<(std::ostream& os, const error& error)
  {
    return os << "\x1b[31m#<error \"" << error.what() << "\">\x1b[0m";
  }

  std::ostream& operator<<(std::ostream& os, const warning& warning)
  {
    return os << "\x1b[33m#<warning \"" << warning.what() << "\">\x1b[0m";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP

