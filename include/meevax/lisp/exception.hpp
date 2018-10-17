#ifndef INCLUDED_MEEVAX_LISP_EXCEPTION_HPP
#define INCLUDED_MEEVAX_LISP_EXCEPTION_HPP

#include <iostream>
#include <stdexcept>
#include <string>

#include <meevax/lisp/cell.hpp>

#define error(...) \
  "\e[32m(\e[1;31merror \e[32m(file \e[36m" << __FILE__ << "\e[32m) (line \e[36m" << __LINE__ << ") (" << __VA_ARGS__ << ")\e[32m)\e[0m"

namespace meevax::lisp
{
  // XXX UGLY CODE!!!
  class exception
    : public std::runtime_error
  {
    const std::string file_;
    std::size_t line_;

  public:
    exception(const std::string& what, const std::string& file, std::size_t line)
      : std::runtime_error {what},
        file_ {file},
        line_ {line}
    {}

    decltype(auto) file() const noexcept
    {
      return file_;
    }

    decltype(auto) line() const noexcept
    {
      return line_;
    }
  };

  decltype(auto) operator<<(std::ostream& os, const exception& exception)
  {
    static const auto clear {"\e[0m"};
    static const auto red   {"\e[0m\e[31m"};
    static const auto cyan  {"\e[0m\e[36m"};

    os << "(" << red << "error" << clear << "\n"
       << "  (file " << cyan << "\"" << exception.file() << "\"" << clear << ")\n"
       << "  (line " << cyan << "\"" << exception.line() << "\"" << clear << ")\n"
       << "  (what " << cyan << "\"" << exception.what() << "\"" << clear << "))\n";

    return os;
  }
} // meevax::lisp

#define generate_exception(...) meevax::lisp::exception {__VA_ARGS__, __FILE__, __LINE__}

#endif // INCLUDED_MEEVAX_LISP_EXCEPTION_HPP

