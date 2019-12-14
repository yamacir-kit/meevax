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

  #define MAKE_MEEVAX_KERNEL_BOOLEAN_T make<boolean>(true)
  #define MAKE_MEEVAX_KERNEL_BOOLEAN_F make<boolean>(false)

  #define DECLARE_MEEVAX_KERNEL_BOOLEAN_T(...) const object true_object __VA_ARGS__
  #define DECLARE_MEEVAX_KERNEL_BOOLEAN_F(...) const object false_object __VA_ARGS__

  #define DEFINE_MEEVAX_KERNEL_BOOLEAN_T(...) DECLARE_MEEVAX_KERNEL_BOOLEAN_T({__VA_ARGS__})
  #define DEFINE_MEEVAX_KERNEL_BOOLEAN_F(...) DECLARE_MEEVAX_KERNEL_BOOLEAN_F({__VA_ARGS__})

  #ifndef MEEVAX_KERNEL_HEADER_ONLY
  extern "C" DECLARE_MEEVAX_KERNEL_BOOLEAN_T();
  extern "C" DECLARE_MEEVAX_KERNEL_BOOLEAN_F();
  #else
  static DEFINE_MEEVAX_KERNEL_BOOLEAN_T(MAKE_MEEVAX_KERNEL_BOOLEAN_T);
  static DEFINE_MEEVAX_KERNEL_BOOLEAN_F(MAKE_MEEVAX_KERNEL_BOOLEAN_F);
  #endif // MEEVAX_KERNEL_HEADER_ONLY
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_BOOLEAN_HPP

