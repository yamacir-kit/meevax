#ifndef INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP
#define INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP

#include <iostream>
#include <stdexcept> // std::runtime_error
#include <utility> // std::forward

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
    return os << "#<" << error.what() << ">";
  }

  const cursor undefined {make<exception>("undefined")};
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_EXCEPTION_HPP

