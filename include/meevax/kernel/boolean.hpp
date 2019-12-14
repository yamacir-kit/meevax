#ifndef INCLUDED_MEEVAX_KERNEL_BOOLEAN_HPP
#define INCLUDED_MEEVAX_KERNEL_BOOLEAN_HPP

#include <meevax/kernel/object.hpp>

namespace meevax::kernel
{
  struct boolean
  {
    const bool data;

    explicit constexpr operator bool() const noexcept
    {
      return data;
    }
  };

  auto operator<<(std::ostream& os, const boolean& boolean)
    -> decltype(os)
  {
    return os << highlight::simple_datum
              << "#"
              << std::boolalpha
              << static_cast<bool>(boolean)
              << attribute::normal;
  }

  #ifndef MEEVAX_KERNEL_HEADER_ONLY
  extern "C" const object true_object, false_object;
  #else
  static const object true_object {make<boolean>(true)},
                     false_object {make<boolean>(false)};
  #endif // MEEVAX_KERNEL_HEADER_ONLY
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_BOOLEAN_HPP

