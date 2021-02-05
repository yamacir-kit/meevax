#ifndef INCLUDED_MEEVAX_KERNEL_PATH_HPP
#define INCLUDED_MEEVAX_KERNEL_PATH_HPP

#ifdef __cpp_lib_filesystem
#include <filesystem>
#define STD_FILESYSTEM_PATH std::filesystem::path
#else
#include <experimental/filesystem>
#define STD_FILESYSTEM_PATH std::experimental::filesystem::path
#endif

#include <meevax/kernel/object.hpp>

namespace meevax
{
inline namespace kernel
{
  struct path : public STD_FILESYSTEM_PATH
  {
    template <typename... Ts>
    explicit constexpr path(Ts&&... xs)
      : STD_FILESYSTEM_PATH { std::forward<decltype(xs)>(xs)... }
    {}
  };

  auto operator <<(output_port & port, path const&) -> output_port &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PATH_HPP
