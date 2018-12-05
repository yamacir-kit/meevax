#ifndef INCLUDED_MEEVAX_LISP_EXCEPTION_HPP
#define INCLUDED_MEEVAX_LISP_EXCEPTION_HPP

#include <iostream>
#include <stdexcept>
#include <string>

namespace meevax::lisp
{
  class [[deprecated]] exception
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
    static const auto clear {"\x1B[0m"};
    static const auto red   {"\x1B[0m\x1B[31m"};
    static const auto cyan  {"\x1B[0m\x1B[36m"};

    os << "(" << red << "error" << clear << "\n"
       << "  (file " << cyan << "\"" << exception.file() << "\"" << clear << ")\n"
       << "  (line " << cyan << "\"" << exception.line() << "\"" << clear << ")\n"
       << "  (what " << cyan << "\"" << exception.what() << "\"" << clear << "))\n";

    return os;
  }
} // meevax::lisp

#define generate_exception(...) meevax::lisp::exception {__VA_ARGS__, __FILE__, __LINE__}

#endif // INCLUDED_MEEVAX_LISP_EXCEPTION_HPP

