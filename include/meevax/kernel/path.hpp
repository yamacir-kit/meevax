#ifndef INCLUDED_MEEVAX_KERNEL_PATH_HPP
#define INCLUDED_MEEVAX_KERNEL_PATH_HPP

#ifdef __cpp_lib_filesystem
  #include <filesystem>
  #define STD_FILESYSTEM_PATH std::filesystem::path
#else
  #include <experimental/filesystem>
  #define STD_FILESYSTEM_PATH std::experimental::filesystem::path
#endif

namespace meevax
{
inline namespace kernel
{
  struct path
    : public STD_FILESYSTEM_PATH
  {
      using STD_FILESYSTEM_PATH::path;

    template <typename... Ts>
    explicit constexpr path(Ts&&... xs)
      : STD_FILESYSTEM_PATH { std::forward<decltype(xs)>(xs)... }
    {}

    friend auto operator<<(std::ostream& port, const path&) -> decltype(port);
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PATH_HPP
