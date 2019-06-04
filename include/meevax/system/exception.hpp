#ifndef INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP
#define INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP

#include <sstream>
#include <stdexcept> // std::runtime_error
#include <type_traits> // std::is_constructible

// exception
//  |-- error
//  |    `-- syntax_error
//  `-- warning

namespace meevax::system
{
  struct exception
    : public std::runtime_error
  {
    template <typename S,
              typename = typename std::enable_if<
                           std::is_constructible<std::string, S>::value
                         >::type>
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

  struct error
    : public exception
  {
    template <typename... Ts>
    constexpr error(Ts&&... args)
      : exception {std::forward<Ts>(args)...}
    {}
  };

  // struct syntax_error
  //   : public error
  // {
  //   template <typename... Ts>
  //   constexpr syntax_error(Ts&&... args)
  //     : error {std::forward<Ts>(args)...}
  //   {}
  // };

  struct warning
    : public exception
  {
    template <typename... Ts>
    constexpr warning(Ts&&... args)
      : exception {std::forward<Ts>(args)...}
    {}
  };

  std::ostream& operator<<(std::ostream&, const exception&);
  std::ostream& operator<<(std::ostream&, const error&);
  // std::ostream& operator<<(std::ostream&, const syntax_error&);
  std::ostream& operator<<(std::ostream&, const warning&);
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP

