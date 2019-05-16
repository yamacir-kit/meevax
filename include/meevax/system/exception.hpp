#ifndef INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP
#define INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP

#include <stdexcept> // std::runtime_error

// exception
//  |-- error
//  `-- warning

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

  struct error
    : public exception
  {
    template <typename... Ts>
    constexpr error(Ts&&... args)
      : exception {std::forward<Ts>(args)...}
    {}
  };

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
  std::ostream& operator<<(std::ostream&, const warning&);
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP

