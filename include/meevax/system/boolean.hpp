#ifndef INCLUDED_MEEVAX_SYSTEM_BOOLEAN_HPP
#define INCLUDED_MEEVAX_SYSTEM_BOOLEAN_HPP

#include <meevax/system/object.hpp>

namespace meevax::system
{
  extern "C" const object true_object, false_object;

  // template <auto Value>
  // std::ostream& operator<<(std::ostream& os, std::bool_constant<Value>)
  // {
  //   return os << highlight::simple_datum << "#" << std::boolalpha << Value << attribute::normal;
  // }

  struct boolean
  {
    bool data;

    explicit operator bool() const noexcept
    {
      return data;
    }
  };

  auto operator<<(std::ostream& os, const boolean& boolean)
    -> decltype(os)
  {
    return os << highlight::simple_datum << "#" << std::boolalpha << static_cast<bool>(boolean) << attribute::normal;
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_BOOLEAN_HPP

