#ifndef INCLUDED_MEEVAX_KERNEL_BOOLEAN_HPP
#define INCLUDED_MEEVAX_KERNEL_BOOLEAN_HPP

#include <meevax/kernel/object.hpp>

namespace meevax::kernel
{
  struct boolean
  {
    const bool value;

    constexpr operator bool() const noexcept
    {
      return value;
    }

    friend auto operator==(const boolean& lhs, const boolean& rhs)
    {
      return lhs.value == rhs.value;
    }

    friend auto operator<<(std::ostream& os, const boolean& datum)
      -> decltype(os)
    {
      return os << console::cyan << "#" << std::boolalpha << datum.value
                << console::reset;
    }
  };

  static const object t {make<boolean>(true)};
  static const object f {make<boolean>(false)};

  inline const object& convert(bool datum)
  {
    return datum ? t : f;
  }
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_BOOLEAN_HPP

