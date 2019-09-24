#ifndef INCLUDED_MEEVAX_KERNEL_PATH_HPP
#define INCLUDED_MEEVAX_KERNEL_PATH_HPP

#include <experimental/filesystem>

#include <meevax/kernel/object.hpp>

namespace meevax::kernel
{
  struct path
    : public std::experimental::filesystem::path
  {
    template <typename... Ts>
    explicit constexpr path(Ts&&... operands)
      : std::experimental::filesystem::path {std::forward<decltype(operands)>(operands)...}
    {}
  };

  auto operator<<(std::ostream& os, const path& path)
    -> decltype(os)
  {
    return os << highlight::syntax << "#("
              << highlight::constructor << "path"
              << highlight::simple_datum << " \"" << path.c_str() << "\""
              << highlight::syntax << ")"
              << attribute::normal;
  }
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_PATH_HPP

