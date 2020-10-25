#ifndef INCLUDED_MEEVAX_KERNEL_PATH_HPP
#define INCLUDED_MEEVAX_KERNEL_PATH_HPP

#include <experimental/filesystem>

namespace meevax { inline namespace kernel
{
  struct path
    : public std::experimental::filesystem::path
  {
    using std::experimental::filesystem::path::path;

    friend auto operator<<(std::ostream& port, const path&) -> decltype(port);
  };
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_PATH_HPP
