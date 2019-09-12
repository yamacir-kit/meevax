#ifndef INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP
#define INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP

#include <sstream>

#include <meevax/concepts/requires.hpp>
#include <meevax/system/object.hpp>

// exception
//  |-- error
//  |    |-- reader_error
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

    template <typename... Objects>
    exception(Objects&&... objects)
      : std::runtime_error {write(
          std::ostringstream {}, std::forward<Objects>(objects)...
        ).str()}
    {}
  };

  DERIVE(error, public, exception)

  enum class category
  {
    pair, parentheses,
  };

  template <category>
  DERIVE(read_error, public, error)

  DERIVE(syntax_error, public, error)

  std::ostream& operator<<(std::ostream& os, const exception& exception)
  {
    return os << highlight::syntax << "#("
              << highlight::constructor << "exception"
              << highlight::simple_datum << " \"" << exception.what() << "\""
              << highlight::syntax << ")"
              << attribute::normal;
  }

  std::ostream& operator<<(std::ostream& os, const error& error)
  {
    return os << highlight::syntax << "#("
              << highlight::constructor << "error"
              << highlight::simple_datum << " \"" << error.what() << "\""
              << highlight::syntax << ")"
              << attribute::normal;
  }

  template <category Category>
  std::ostream& operator<<(std::ostream& os, const read_error<Category>& error)
  {
    os << "\x1b[35m" << "#("
       << "\x1b[32m" << "read-error"
       << "\x1b[0m " << "#;(category ";

    switch (Category)
    {
    case category::pair:
      os << "pair";
      break;

    case category::parentheses:
      os << "parentheses";
      break;

    default:
      os << "unknown";
      break;
    }

    return os << ") "
              << "\x1b[36m" << "\"" << error.what() << "\""
              << "\x1b[35m" << ")"
              << "\x1b[0m";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP

