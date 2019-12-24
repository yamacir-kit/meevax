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

    friend auto operator<<(std::ostream& os, const boolean& boolean)
      -> decltype(os)
    {
      return os << highlight::simple_datum
                << "#"
                << std::boolalpha
                << static_cast<bool>(boolean)
                << attribute::normal;
    }
  };

  // static const object true_object {make<boolean>(true)};
  // static const object false_object {make<boolean>(false)};
  extern "C" const object true_object;
  extern "C" const object false_object;
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_BOOLEAN_HPP

